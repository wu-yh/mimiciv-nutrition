-- select patients meeting the inclusion criteria
WITH ig_stay
AS (
	SELECT DISTINCT stay_id
	FROM mimiciv_icu.ingredientevents
	)
	,d_s1 -- inclusion criteria: 1. there were records in the ingredientevents table in MIMIC-IV v2.0
AS (
	SELECT de.*
	FROM ig_stay
	LEFT JOIN icustay_detail de ON ig_stay.stay_id = de.stay_id
	)
	,svc -- add finish time in the services table
AS (
	SELECT svc.*
		,LEAD(svc.transfertime, 1) OVER (
			PARTITION BY svc.hadm_id ORDER BY svc.transfertime
			) finishtime
	FROM mimiciv_hosp.services svc
	)
	,cu -- add care unit and its transfertime (starttime) and finishtime from the services table to the table d_s1
AS (
	SELECT seq.*
		,CASE 
			WHEN (
					seq.careunit_seq = 1
					AND seq.transfertime < seq.icu_intime
					AND (
						seq.finishtime IS NULL
						OR seq.finishtime > DATETIME_ADD(seq.icu_intime, INTERVAL '1' HOUR)
						)
					)
				OR (
					seq.transfertime >= seq.icu_intime
					AND seq.transfertime < DATETIME_SUB(seq.icu_outtime, INTERVAL '1' HOUR)
					)
				THEN seq.curr_service
			ELSE NULL
			END careunit
	FROM (
		SELECT s1.*
			,DENSE_RANK() OVER (
				PARTITION BY s1.stay_id ORDER BY s1.transfertime_bf DESC NULLS LAST
				) careunit_seq
		FROM (
			SELECT d1.*
				,svc.curr_service
				,svc.transfertime
				,svc.finishtime
				,CASE 
					WHEN svc.transfertime < d1.icu_intime
						THEN svc.transfertime
					ELSE NULL
					END transfertime_bf
			FROM d_s1 d1
			LEFT JOIN svc ON d1.hadm_id = svc.hadm_id
			) s1
		) seq
	)
	-- select TSURG, SURG
	-- and sort admissions to ICU
	,d_s2
AS (
	SELECT cu.subject_id
		,cu.hadm_id
		,cu.stay_id
		,cu.gender
		,cu.dod
		,cu.admittime
		,cu.dischtime
		,cu.los_hospital
		,cu.admission_age
		,cu.race
		,cu.hospital_expire_flag
		,cu.hospstay_seq
		,cu.first_hosp_stay
		,cu.icu_intime
		,cu.icu_outtime
		,cu.los_icu
		,cu.icustay_seq
		,cu.first_icu_stay
		,cu.careunit
		,cu.transfertime
		,cu.finishtime
		,DENSE_RANK() OVER (
			PARTITION BY cu.subject_id ORDER BY cu.transfertime
			) service_seq
	FROM cu
	WHERE cu.careunit IN (
			'TSURG'
			,'SURG'
			)
	)
	,an_tot -- artificial nutrition (en and/or pn) in the first five days of ICU
AS (
	SELECT de.stay_id
		,SUM(CASE 
				WHEN ig.starttime > de.icu_intime
					AND ig.starttime < datetime_add(de.icu_intime, INTERVAL '5' DAY)
					AND ig.itemid IN (
						226221
						,227079
						)
					THEN ig.amount
				ELSE 0
				END) an_tot
	FROM icustay_detail de
	LEFT JOIN mimiciv_icu.ingredientevents ig ON de.stay_id = ig.stay_id
	GROUP BY de.stay_id
	)
	,d0
AS (
	SELECT s2.*
	FROM d_s2 s2
	LEFT JOIN an_tot AS at ON s2.stay_id = at.stay_id
	WHERE s2.admission_age >= 18 -- 2. age equal to or greater than 18 years old (after query, we find all icu patients were >= 18)
		AND s2.service_seq = 1 -- 3. first ICU admission
		AND s2.los_icu >= 4 -- 4. ICU stay equal to or more than 4 days
		AND at.an_tot > 0 -- 5. administration of artificial nutrition (en and/or pn) within 5 days of ICU admission
	)
	,
	-- num of patients in icustay_detail or icustays = 53569
	-- num of patients in ingredientevents = 53322
	-- num of patients meeting all inclusion criteria = 1247
	--
	-- add a "day" column to count days from admission to to discharge from ICU (up to 10 days)
d
AS (
	SELECT d0.*
		,num_days.DAY
	FROM d0
	CROSS JOIN (
		SELECT 1 AS DAY
		
		UNION ALL
		
		SELECT 2
		
		UNION ALL
		
		SELECT 3
		
		UNION ALL
		
		SELECT 4
		
		UNION ALL
		
		SELECT 5
		
		UNION ALL
		
		SELECT 6
		
		UNION ALL
		
		SELECT 7
		
		UNION ALL
		
		SELECT 8
		
		UNION ALL
		
		SELECT 9
		
		UNION ALL
		
		SELECT 10
		) num_days
	WHERE num_days.DAY <= LEAST(CEIL(d0.los_icu), 10)
	)
	,
	--
	-- total carlories(kcal) and protein(g) intake each day within 10 days
ntr_tot
AS (
	SELECT d.stay_id
		,d.DAY
		,COALESCE(s1.cal_tot, 0) cal_tot0
		,COALESCE(s1.pr_tot, 0) pr_tot
	FROM d
	LEFT JOIN (
		SELECT ig.stay_id
			,ig.DAY
			,SUM(CASE 
					WHEN ig.itemid IN (
							226060
							,220412
							)
						THEN ig.amount
					ELSE 0
					END) cal_tot
			,SUM(CASE 
					WHEN ig.itemid = 220454
						THEN ig.amount
					ELSE 0
					END) pr_tot
		FROM (
			SELECT d0.stay_id
				,ig0.itemid
				,ig0.amount
				,CEIL(datetime_diff(ig0.starttime, d0.icu_intime, 'HOUR') / 24) AS DAY
			FROM d0
			LEFT JOIN mimiciv_icu.ingredientevents ig0 ON d0.stay_id = ig0.stay_id
				AND ig0.starttime > d0.icu_intime
			) ig -- convert starttime to day
		GROUP BY ig.stay_id
			,ig.DAY
		) s1 ON d.stay_id = s1.stay_id
		AND d.DAY = s1.DAY
	)
	,
	--
	-- enteral and parental calories(kcal) intake each day within 10 days
ig -- convert starttime to day
AS (
	SELECT d0.stay_id
		,ig0.itemid
		,ig0.amount
		,ig0.orderid
		,CEIL(datetime_diff(ig0.starttime, d0.icu_intime, 'HOUR') / 24) AS DAY
	FROM d0
	LEFT JOIN mimiciv_icu.ingredientevents ig0 ON d0.stay_id = ig0.stay_id
		AND ig0.starttime > d0.icu_intime
	)
	,ntr_comp
AS (
	SELECT d.stay_id
		,d.DAY
		,COALESCE(s1.cal_en, 0) cal_en
		,COALESCE(s1.cal_pn, 0) cal_pn0
	FROM d
	LEFT JOIN (
		SELECT ig.stay_id
			,ig.DAY
			,SUM(CASE 
					WHEN ig1.itemid = 226221
						AND ig.itemid IN (
							226060
							,220412
							)
						THEN ig.amount
					ELSE 0
					END) cal_en
			,SUM(CASE 
					WHEN ig1.itemid = 227079
						AND ig.itemid IN (
							226060
							,220412
							)
						THEN ig.amount
					ELSE 0
					END) cal_pn
		FROM ig
		LEFT JOIN ig ig1 ON ig.orderid = ig1.orderid
		GROUP BY ig.stay_id
			,ig.DAY
		) s1 ON d.stay_id = s1.stay_id
		AND d.DAY = s1.DAY
	)
	,
	--
	-- some en and pn don't have calorie information
	-- we add this part of calorie assuming an energy density of 1kcal/ml
	-- after query, we find only some pn don't have calorie information
orderid
AS (
	SELECT orderid
	FROM ig
	GROUP BY orderid
	HAVING SUM(CASE 
				WHEN itemid IN (
						226221
						,227079
						)
					THEN 1
				ELSE 0
				END) > 0
		AND SUM(CASE 
				WHEN itemid IN (
						226060
						,220412
						)
					THEN 1
				ELSE 0
				END) = 0
	)
	,cal_supp
AS (
	SELECT ig.stay_id
		,ig.day
		,SUM(CASE 
				WHEN ig.itemid = 226221
					THEN ig.amount
				ELSE 0
				END) cal_supp_en
		,SUM(CASE 
				WHEN ig.itemid = 227079
					THEN ig.amount
				ELSE 0
				END) cal_supp_pn
	FROM ig
	INNER JOIN orderid oi ON ig.orderid = oi.orderid
	GROUP BY ig.stay_id
		,ig.day
	)
	,
	--
	-- average blood glucose(mg/dL), BUN(mg/dL), hco3(mEq/L, i.e., mmol/L) each day within 10 days
	-- from chemistry table(labevents table)
metab_1
AS (
	SELECT d.stay_id
		,d.DAY
		,s1.glu
		,s1.bun
		,s1.hco3
	FROM d
	LEFT JOIN (
		SELECT chem.stay_id
			,chem.DAY
			,AVG(chem.glucose) glu
			,AVG(chem.bun) bun
			,AVG(chem.bicarbonate) hco3
		FROM (
			SELECT d0.stay_id
				,chem0.glucose
				,chem0.bun
				,chem0.bicarbonate
				,CEIL(datetime_diff(chem0.charttime, d0.icu_intime, 'HOUR') / 24) AS DAY
			FROM d0
			LEFT JOIN chemistry chem0 ON d0.subject_id = chem0.subject_id
				AND chem0.charttime > d0.icu_intime
			) chem -- convert charttime to day
		GROUP BY chem.stay_id
			,chem.DAY
		) s1 ON d.stay_id = s1.stay_id
		AND d.DAY = s1.DAY
	)
	,
	--
	-- insulin(unit) use each day within 10 days
	-- from inputevents table
ins
AS (
	SELECT d.stay_id
		,d.DAY
		,COALESCE(s1.ins, 0) ins
	FROM d
	LEFT JOIN (
		SELECT ip.stay_id
			,ip.DAY
			,SUM(CASE 
					WHEN ip.itemid IN (
							223257
							,223258
							,223259
							,223260
							,223261
							,223262
							,229299
							,229619
							)
						THEN ip.amount
					ELSE 0
					END) AS ins
		FROM (
			SELECT d0.stay_id
				,ip0.itemid
				,ip0.amount
				,CEIL(datetime_diff(ip0.starttime, d0.icu_intime, 'HOUR') / 24) AS DAY
			FROM d0
			LEFT JOIN mimiciv_icu.inputevents ip0 ON d0.stay_id = ip0.stay_id
				AND ip0.starttime > d0.icu_intime
			) ip -- convert starttime to day
		GROUP BY ip.stay_id
			,ip.DAY
		) s1 ON d.stay_id = s1.stay_id
		AND d.DAY = s1.DAY
	)
	,
	--
	-- extract chemistry: triglyceride(mg/dL), phosphate(mg/dL)
biochem
AS (
	SELECT MAX(subject_id) AS subject_id
		,MAX(charttime) AS charttime
		,le.specimen_id
		,MAX(CASE 
				WHEN itemid = 51000
					AND valuenum <= 1000
					THEN valuenum
				ELSE NULL
				END) AS tg
		,MAX(CASE 
				WHEN itemid = 50970
					AND valuenum <= 20
					THEN valuenum
				ELSE NULL
				END) AS po4
	FROM mimiciv_hosp.labevents le
	WHERE le.itemid IN (
			51000
			,50970
			)
		AND valuenum IS NOT NULL
		AND valuenum > 0
	GROUP BY le.specimen_id
	)
	,
	--
	-- triglyceride(mg/dL), phosphate(mg/dL) each day within 10 days
	-- from labevents table
metab_2
AS (
	SELECT d.stay_id
		,d.DAY
		,s1.tg
		,s1.po4
	FROM d
	LEFT JOIN (
		SELECT biochem.stay_id
			,biochem.DAY
			,AVG(biochem.tg) tg
			,AVG(biochem.po4) po4
		FROM (
			SELECT d0.stay_id
				,biochem0.tg
				,biochem0.po4
				,CEIL(datetime_diff(biochem0.charttime, d0.icu_intime, 'HOUR') / 24) AS DAY
			FROM d0
			LEFT JOIN biochem biochem0 ON d0.subject_id = biochem0.subject_id
				AND biochem0.charttime > d0.icu_intime
			) biochem -- convert charttime to day
		GROUP BY biochem.stay_id
			,biochem.DAY
		) s1 ON d.stay_id = s1.stay_id
		AND d.DAY = s1.DAY
	)
	,
	--
	-- ph, paco2(mmHg) each day within 10 days
	-- from bg table
metab_3
AS (
	SELECT d.stay_id
		,d.DAY
		,s1.ph
		,s1.paco2
	FROM d
	LEFT JOIN (
		SELECT bg.stay_id
			,bg.DAY
			,AVG(bg.ph) ph
			,AVG(bg.paco2) paco2
		FROM (
			SELECT d0.stay_id
				,bg0.ph
				,bg0.pco2 paco2
				,CEIL(datetime_diff(bg0.charttime, d0.icu_intime, 'HOUR') / 24) AS DAY
			FROM d0
			LEFT JOIN bg bg0 ON d0.subject_id = bg0.subject_id
				AND bg0.charttime > d0.icu_intime
			WHERE bg0.specimen_pred = 'ART.'
			) bg -- convert charttime to day
		GROUP BY bg.stay_id
			,bg.DAY
		) s1 ON d.stay_id = s1.stay_id
		AND d.DAY = s1.DAY
	)
	,
	--
	-- weight(kg), height(m), BMI(kg/m2) at admission
wt
AS (
	SELECT d0.stay_id
		,COALESCE(wt.weight_admit, wt.weight) wt
	FROM d0
	LEFT JOIN first_day_weight wt ON d0.stay_id = wt.stay_id
	)
	,ht -- from height table
AS (
	SELECT d0.stay_id
		,AVG(height.height) / 100 ht
	FROM d0
	LEFT JOIN height ON d0.stay_id = height.stay_id
	GROUP BY d0.stay_id
	)
	,ht1 -- from omr table (unit of height is inch, 1inch*0.0254 = 1m)
AS (
	SELECT d0.stay_id
		,AVG(ht.ht) ht
	FROM d0
	LEFT JOIN (
		SELECT subject_id
			,0.0254 * (result_value::NUMERIC) ht
		FROM mimiciv_hosp.omr
		WHERE result_name IN (
				'Height'
				,'Height (Inches)'
				)
		) ht ON d0.subject_id = ht.subject_id
	GROUP BY d0.stay_id
	)
	,demo
AS (
	SELECT wt.stay_id
		,COALESCE(ht.ht, ht1.ht) ht
		,wt.wt
		,wt.wt / (COALESCE(ht.ht, ht1.ht) * COALESCE(ht.ht, ht1.ht)) bmi
	FROM wt
	LEFT JOIN ht ON wt.stay_id = ht.stay_id
	LEFT JOIN ht1 ON wt.stay_id = ht1.stay_id
	)
	,
	--
	-- vasopressors at admission
vasopressor
AS (
	SELECT d0.stay_id
		,MAX(CASE 
				WHEN va.stay_id IS NOT NULL
					THEN 1
				ELSE 0
				END) vaso
	FROM d0
	LEFT JOIN vasoactive_agent va ON d0.stay_id = va.stay_id
		AND va.starttime < DATETIME_ADD(d0.icu_intime, INTERVAL '1' DAY)
	GROUP BY d0.stay_id
	)
	,
	--
	-- sepsis at admission
sepsis
AS (
	SELECT d0.stay_id
		,CASE 
			WHEN s3.stay_id IS NOT NULL
				THEN 1
			ELSE 0
			END sepsis
	FROM d0
	LEFT JOIN sepsis3 s3 ON d0.stay_id = s3.stay_id
		AND s3.suspected_infection_time < DATETIME_ADD(d0.icu_intime, INTERVAL '1' DAY)
		AND s3.sofa_time < DATETIME_ADD(d0.icu_intime, INTERVAL '1' DAY)
	)
	,
	--
	-- imv/nimv at admission
mv_flag
AS (
	SELECT d0.stay_id
		,MAX(CASE 
				WHEN v.ventilation_status IN (
						'Trach'
						,'InvasiveVent'
						)
					THEN 1
				ELSE 0
				END) imv_flag
		,MAX(CASE 
				WHEN v.ventilation_status = 'NonInvasiveVent'
					THEN 1
				ELSE 0
				END) nimv_flag
	FROM d0
	LEFT JOIN ventilation v ON d0.stay_id = v.stay_id
		AND v.starttime < DATETIME_ADD(d0.icu_intime, INTERVAL '1' DAY)
	GROUP BY d0.stay_id
	)
	,
	--
	-- imv/nimv duration(hour)
mv_dur
AS (
	SELECT d0.stay_id
		,SUM(CASE 
				WHEN v.ventilation_status IN (
						'Trach'
						,'InvasiveVent'
						)
					THEN datetime_diff(v.endtime, v.starttime, 'MINUTE') / 60
				ELSE 0
				END) imv_dur
		,SUM(CASE 
				WHEN v.ventilation_status = 'NonInvasiveVent'
					THEN datetime_diff(v.endtime, v.starttime, 'MINUTE') / 60
				ELSE 0
				END) nimv_dur
	FROM d0
	LEFT JOIN ventilation v ON d0.stay_id = v.stay_id
	GROUP BY d0.stay_id
	)
	,
	--
	-- rrt at admission
rrt
AS (
	SELECT d0.stay_id
		,COALESCE(MAX(r.dialysis_active), 0) rrt
	FROM d0
	LEFT JOIN rrt r ON d0.stay_id = r.stay_id
		AND r.charttime < DATETIME_ADD(d0.icu_intime, INTERVAL '1' DAY)
	GROUP BY d0.stay_id
	)
	,
	--
	-- time(hour) to first EN after admission
first_en
AS (
	SELECT d0.stay_id
		,CASE 
			WHEN datetime_diff(s1.starttime, d0.icu_intime, 'MINUTE') IS NOT NULL
				THEN GREATEST(datetime_diff(s1.starttime, d0.icu_intime, 'MINUTE') / 60, 0)
			END first_en
	FROM d0
	LEFT JOIN (
		SELECT stay_id
			,MIN(starttime) starttime
		FROM mimiciv_icu.ingredientevents
		WHERE itemid = 226221
		GROUP BY stay_id
		) s1 ON d0.stay_id = s1.stay_id
	)
	,
	--
	-- time(hour) to first PN after admission
first_pn
AS (
	SELECT d0.stay_id
		,CASE 
			WHEN datetime_diff(s1.starttime, d0.icu_intime, 'MINUTE') IS NOT NULL
				THEN GREATEST(datetime_diff(s1.starttime, d0.icu_intime, 'MINUTE') / 60, 0)
			END first_pn
	FROM d0
	LEFT JOIN (
		SELECT stay_id
			,MIN(starttime) starttime
		FROM mimiciv_icu.ingredientevents
		WHERE itemid = 227079
		GROUP BY stay_id
		) s1 ON d0.stay_id = s1.stay_id
	)
	,
	--
	-- new onset infection in ICU
inf
AS (
	SELECT d0.stay_id
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen IN (
						'BLOOD CULTURE'
						,'BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)'
						,'BLOOD CULTURE (POST-MORTEM)'
						)
					THEN 1
				ELSE 0
				END) blood_flag
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen IN (
						'BRONCHIAL BRUSH'
						,'BRONCHIAL BRUSH - PROTECTED'
						,'BRONCHIAL WASHINGS'
						,'BRONCHOALVEOLAR LAVAGE'
						,'Mini-BAL'
						,'SPUTUM'
						,'THROAT CULTURE'
						,'THROAT FOR STREP'
						,'TRACHEAL ASPIRATE'
						)
					THEN 1
				ELSE 0
				END) resp_flag
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen = 'PLEURAL FLUID'
					THEN 1
				ELSE 0
				END) pleural_flag
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen IN (
						'URINE'
						,'URINE,KIDNEY'
						,'URINE,SUPRAPUBIC ASPIRATE'
						)
					THEN 1
				ELSE 0
				END) urine_flag
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen = 'FLUID WOUND'
					THEN 1
				ELSE 0
				END) incision_flag
	FROM d0
	LEFT JOIN suspicion_of_infection soi ON d0.hadm_id = soi.hadm_id
		AND soi.culture_time >= d0.icu_intime
		AND soi.culture_time <= d0.icu_outtime
	GROUP BY d0.stay_id
	)
	,
	--
	-- ICU, hospital mortality
expire_flag
AS (
	SELECT d0.stay_id
		,CASE 
			WHEN adm.deathtime BETWEEN d0.icu_intime
					AND d0.icu_outtime
				THEN 1
			WHEN adm.deathtime <= d0.icu_intime -- sometimes there are typographical errors in the death date
				THEN 1
			WHEN adm.dischtime <= d0.icu_outtime
				AND adm.discharge_location = 'DIED'
				THEN 1
			ELSE 0
			END icu_expire_flag
		,CASE 
			WHEN adm.hospital_expire_flag = 1
				OR adm.discharge_location = 'DIED'
				THEN 1
			ELSE 0
			END hospital_expire_flag
	FROM d0
	LEFT JOIN mimiciv_hosp.admissions adm ON d0.hadm_id = adm.hadm_id
	)
	,
	--
	-- 28 day and 90 day survival (days counted from ICU admission)
survival
AS (
	SELECT d0.stay_id
		,datetime_diff(pa.dod, d0.icu_intime, 'DAY') surv_time
		,CASE 
			WHEN datetime_diff(pa.dod, d0.icu_intime, 'DAY') <= 28
				THEN 1
			ELSE 0
			END expire_flag_28d
		,CASE 
			WHEN datetime_diff(pa.dod, d0.icu_intime, 'DAY') <= 90
				THEN 1
			ELSE 0
			END expire_flag_90d
		,LEAST(datetime_diff(pa.dod, d0.icu_intime, 'DAY'), 28) surv_time_28d
		,LEAST(datetime_diff(pa.dod, d0.icu_intime, 'DAY'), 90) surv_time_90d
	FROM d0
	LEFT JOIN mimiciv_hosp.patients pa ON d0.subject_id = pa.subject_id
	)
--
-- the complete table
SELECT d.stay_id AS ID
	,d.admission_age age
	,d.gender
	,d.race
	,demo.ht
	,demo.wt
	,demo.bmi
	,charlson.charlson_comorbidity_index cci
	,mnutric.mnutric
	,d.careunit adm_type
	,sapsii.sapsii
	,first_day_sofa.sofa
	,apacheii.apacheii
	,vasopressor.vaso
	,sepsis.sepsis
	,mv_flag.imv_flag
	,mv_flag.nimv_flag
	,rrt.rrt
	,metab_1.glu
	,ins.ins
	,metab_1.bun
	,metab_1.hco3
	,metab_2.tg
	,metab_2.po4
	,metab_3.ph
	,metab_3.paco2
	,first_en.first_en
	,first_pn.first_pn
	,d.day
	,ntr_tot.cal_tot0
	,ntr_comp.cal_en
	,ntr_comp.cal_pn0
	,ntr_tot.pr_tot
	,COALESCE(cal_supp.cal_supp_pn, 0) cal_supp_pn
	,ad.discharge_location dis_loc
	,d.los_hospital los_hosp
	,d.los_icu
	,mv_dur.imv_dur
	,mv_dur.nimv_dur
	,inf.blood_flag
	,inf.resp_flag
	,inf.pleural_flag
	,inf.urine_flag
	,inf.incision_flag
	,expire_flag.icu_expire_flag
	,expire_flag.hospital_expire_flag hosp_expire_flag
	,survival.surv_time
	,survival.expire_flag_28d
	,survival.surv_time_28d
	,survival.expire_flag_90d
	,survival.surv_time_90d
FROM d
LEFT JOIN demo ON d.stay_id = demo.stay_id
LEFT JOIN charlson ON d.hadm_id = charlson.hadm_id
LEFT JOIN mnutric ON d.stay_id = mnutric.stay_id
LEFT JOIN sapsii ON d.stay_id = sapsii.stay_id
LEFT JOIN first_day_sofa ON d.stay_id = first_day_sofa.stay_id
LEFT JOIN apacheii ON d.stay_id = apacheii.stay_id
LEFT JOIN vasopressor ON d.stay_id = vasopressor.stay_id
LEFT JOIN sepsis ON d.stay_id = sepsis.stay_id
LEFT JOIN mv_flag ON d.stay_id = mv_flag.stay_id
LEFT JOIN rrt ON d.stay_id = rrt.stay_id
LEFT JOIN first_en ON d.stay_id = first_en.stay_id
LEFT JOIN first_pn ON d.stay_id = first_pn.stay_id
LEFT JOIN metab_1 ON d.stay_id = metab_1.stay_id
	AND d.DAY = metab_1.DAY
LEFT JOIN ins ON d.stay_id = ins.stay_id
	AND d.DAY = ins.DAY
LEFT JOIN metab_2 ON d.stay_id = metab_2.stay_id
	AND d.DAY = metab_2.DAY
LEFT JOIN metab_3 ON d.stay_id = metab_3.stay_id
	AND d.DAY = metab_3.DAY
LEFT JOIN ntr_tot ON d.stay_id = ntr_tot.stay_id
	AND d.DAY = ntr_tot.DAY
LEFT JOIN ntr_comp ON d.stay_id = ntr_comp.stay_id
	AND d.DAY = ntr_comp.DAY
LEFT JOIN cal_supp ON d.stay_id = cal_supp.stay_id
	AND d.DAY = cal_supp.DAY
LEFT JOIN mimiciv_hosp.admissions ad ON d.hadm_id = ad.hadm_id
LEFT JOIN mv_dur ON d.stay_id = mv_dur.stay_id
LEFT JOIN inf ON d.stay_id = inf.stay_id
LEFT JOIN expire_flag ON d.stay_id = expire_flag.stay_id
LEFT JOIN survival ON d.stay_id = survival.stay_id
ORDER BY d.stay_id
	,d.day