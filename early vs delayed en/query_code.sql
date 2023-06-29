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
	-- select CSURG, VSURG, TSURG, i.e., CTICU (Cardiothoracic Intensive Care Unit) patients
	-- and sort admissions to CTICU
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
			'CSURG'
			,'VSURG'
			,'TSURG'
			)
	)
	,en_tot -- total en administration in the first five days in icu
AS (
	SELECT de.stay_id
		,SUM(CASE 
				WHEN ig.starttime >= de.icu_intime
					AND ig.starttime < DATETIME_ADD(de.icu_intime, INTERVAL '5' DAY)
					AND ig.itemid = 226221
					THEN ig.amount
				ELSE 0
				END) en_tot
	FROM icustay_detail de
	LEFT JOIN mimiciv_icu.ingredientevents ig ON de.stay_id = ig.stay_id
	GROUP BY de.stay_id
	)
	,d
AS (
	SELECT s2.*
	FROM d_s2 s2
	LEFT JOIN en_tot et ON s2.stay_id = et.stay_id
	WHERE s2.admission_age >= 18 -- 2. age equal to or greater than 18 years old (after query, we find all icu patients were >= 18)
		AND s2.service_seq = 1 -- 3. first CTICU admission
		AND s2.los_icu >= 4 -- 4. CTICU stay equal to or more than 4 days
		AND et.en_tot > 0 -- 5. administration of enteral nutrition within 5 days of CTICU admission
	)
	,
	-- num of stay_id in icustay_detail or icustays = 76943
	-- num of stay_id in ingredientevents = 76253
	-- num of stay_id with care unit information in ingredientevents = 76249
	-- num of patients meeting all inclusion criteria = 720
	--
	--total carlories, protein, en, pn within 5 days
ntrd1
AS (
	SELECT d.stay_id
		,SUM(CASE 
				WHEN ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_tot_d1
		,SUM(CASE 
				WHEN ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_tot_d1
		,SUM(CASE 
				WHEN ig.itemid = 226221
					THEN ig.amount
				ELSE 0
				END) en_d1
		,SUM(CASE 
				WHEN ig.itemid = 227079
					THEN ig.amount
				ELSE 0
				END) pn_d1
	FROM d
	LEFT JOIN mimiciv_icu.ingredientevents ig ON d.stay_id = ig.stay_id
		AND ig.starttime >= d.icu_intime
		AND ig.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,ntrd2
AS (
	SELECT d.stay_id
		,SUM(CASE 
				WHEN ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_tot_d2
		,SUM(CASE 
				WHEN ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_tot_d2
		,SUM(CASE 
				WHEN ig.itemid = 226221
					THEN ig.amount
				ELSE 0
				END) en_d2
		,SUM(CASE 
				WHEN ig.itemid = 227079
					THEN ig.amount
				ELSE 0
				END) pn_d2
	FROM d
	LEFT JOIN mimiciv_icu.ingredientevents ig ON d.stay_id = ig.stay_id
		AND ig.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
		AND ig.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,ntrd3
AS (
	SELECT d.stay_id
		,SUM(CASE 
				WHEN ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_tot_d3
		,SUM(CASE 
				WHEN ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_tot_d3
		,SUM(CASE 
				WHEN ig.itemid = 226221
					THEN ig.amount
				ELSE 0
				END) en_d3
		,SUM(CASE 
				WHEN ig.itemid = 227079
					THEN ig.amount
				ELSE 0
				END) pn_d3
	FROM d
	LEFT JOIN mimiciv_icu.ingredientevents ig ON d.stay_id = ig.stay_id
		AND ig.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
		AND ig.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,ntrd4
AS (
	SELECT d.stay_id
		,SUM(CASE 
				WHEN ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_tot_d4
		,SUM(CASE 
				WHEN ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_tot_d4
		,SUM(CASE 
				WHEN ig.itemid = 226221
					THEN ig.amount
				ELSE 0
				END) en_d4
		,SUM(CASE 
				WHEN ig.itemid = 227079
					THEN ig.amount
				ELSE 0
				END) pn_d4
	FROM d
	LEFT JOIN mimiciv_icu.ingredientevents ig ON d.stay_id = ig.stay_id
		AND ig.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
		AND ig.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,ntrd5
AS (
	SELECT d.stay_id
		,SUM(CASE 
				WHEN ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_tot_d5
		,SUM(CASE 
				WHEN ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_tot_d5
		,SUM(CASE 
				WHEN ig.itemid = 226221
					THEN ig.amount
				ELSE 0
				END) en_d5
		,SUM(CASE 
				WHEN ig.itemid = 227079
					THEN ig.amount
				ELSE 0
				END) pn_d5
	FROM d
	LEFT JOIN mimiciv_icu.ingredientevents ig ON d.stay_id = ig.stay_id
		AND ig.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
		AND ig.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- enteral calories and protein, parenteral calories and protein within 5 days
	--
	-- stay_ids of min ( starttime ) group by stay_id >= DATETIME_ADD ( d.icu_intime, INTERVAL '1' DAY ) were deleted in ntrd1_comp_s1
	-- add later in ntrd1_comp as 0
	-- the same as ntrd2_comp_s1, ntrd2_comp and so on
ntrd1_comp_s1
AS (
	SELECT ig.stay_id
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_en_d1
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_pn_d1
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_en_d1
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_pn_d1
	FROM mimiciv_icu.ingredientevents ig
	LEFT JOIN mimiciv_icu.ingredientevents ig2 ON ig.orderid = ig2.orderid
	LEFT JOIN icustay_detail de ON ig.stay_id = de.stay_id
	WHERE ig.starttime >= de.icu_intime
		AND ig.starttime < DATETIME_ADD(de.icu_intime, INTERVAL '1' DAY)
	GROUP BY ig.stay_id
	ORDER BY ig.stay_id
	)
	,ntrd1_comp
AS (
	SELECT d.stay_id
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_en_d1
			END cal_en_d1
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_pn_d1
			END cal_pn_d1
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_en_d1
			END pr_en_d1
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_pn_d1
			END pr_pn_d1
	FROM d
	LEFT JOIN ntrd1_comp_s1 s1 ON d.stay_id = s1.stay_id
	ORDER BY d.stay_id
	)
	,ntrd2_comp_s1
AS (
	SELECT ig.stay_id
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_en_d2
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_pn_d2
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_en_d2
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_pn_d2
	FROM mimiciv_icu.ingredientevents ig
	LEFT JOIN mimiciv_icu.ingredientevents ig2 ON ig.orderid = ig2.orderid
	LEFT JOIN icustay_detail de ON ig.stay_id = de.stay_id
	WHERE ig.starttime >= DATETIME_ADD(de.icu_intime, INTERVAL '1' DAY)
		AND ig.starttime < DATETIME_ADD(de.icu_intime, INTERVAL '2' DAY)
	GROUP BY ig.stay_id
	ORDER BY ig.stay_id
	)
	,ntrd2_comp
AS (
	SELECT d.stay_id
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_en_d2
			END cal_en_d2
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_pn_d2
			END cal_pn_d2
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_en_d2
			END pr_en_d2
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_pn_d2
			END pr_pn_d2
	FROM d
	LEFT JOIN ntrd2_comp_s1 s1 ON d.stay_id = s1.stay_id
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	ORDER BY d.stay_id
	)
	,ntrd3_comp_s1
AS (
	SELECT ig.stay_id
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_en_d3
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_pn_d3
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_en_d3
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_pn_d3
	FROM mimiciv_icu.ingredientevents ig
	LEFT JOIN mimiciv_icu.ingredientevents ig2 ON ig.orderid = ig2.orderid
	LEFT JOIN icustay_detail de ON ig.stay_id = de.stay_id
	WHERE ig.starttime >= DATETIME_ADD(de.icu_intime, INTERVAL '2' DAY)
		AND ig.starttime < DATETIME_ADD(de.icu_intime, INTERVAL '3' DAY)
	GROUP BY ig.stay_id
	ORDER BY ig.stay_id
	)
	,ntrd3_comp
AS (
	SELECT d.stay_id
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_en_d3
			END cal_en_d3
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_pn_d3
			END cal_pn_d3
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_en_d3
			END pr_en_d3
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_pn_d3
			END pr_pn_d3
	FROM d
	LEFT JOIN ntrd3_comp_s1 s1 ON d.stay_id = s1.stay_id
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
	ORDER BY d.stay_id
	)
	,ntrd4_comp_s1
AS (
	SELECT ig.stay_id
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_en_d4
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_pn_d4
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_en_d4
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_pn_d4
	FROM mimiciv_icu.ingredientevents ig
	LEFT JOIN mimiciv_icu.ingredientevents ig2 ON ig.orderid = ig2.orderid
	LEFT JOIN icustay_detail de ON ig.stay_id = de.stay_id
	WHERE ig.starttime >= DATETIME_ADD(de.icu_intime, INTERVAL '3' DAY)
		AND ig.starttime < DATETIME_ADD(de.icu_intime, INTERVAL '4' DAY)
	GROUP BY ig.stay_id
	ORDER BY ig.stay_id
	)
	,ntrd4_comp
AS (
	SELECT d.stay_id
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_en_d4
			END cal_en_d4
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_pn_d4
			END cal_pn_d4
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_en_d4
			END pr_en_d4
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_pn_d4
			END pr_pn_d4
	FROM d
	LEFT JOIN ntrd4_comp_s1 s1 ON d.stay_id = s1.stay_id
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
	ORDER BY d.stay_id
	)
	,ntrd5_comp_s1
AS (
	SELECT ig.stay_id
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_en_d5
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid IN (
						226060
						,220412
						)
					THEN ig.amount
				ELSE 0
				END) cal_pn_d5
		,SUM(CASE 
				WHEN ig2.itemid = 226221
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_en_d5
		,SUM(CASE 
				WHEN ig2.itemid = 227079
					AND ig.itemid = 220454
					THEN ig.amount
				ELSE 0
				END) pr_pn_d5
	FROM mimiciv_icu.ingredientevents ig
	LEFT JOIN mimiciv_icu.ingredientevents ig2 ON ig.orderid = ig2.orderid
	LEFT JOIN icustay_detail de ON ig.stay_id = de.stay_id
	WHERE ig.starttime >= DATETIME_ADD(de.icu_intime, INTERVAL '4' DAY)
		AND ig.starttime < DATETIME_ADD(de.icu_intime, INTERVAL '5' DAY)
	GROUP BY ig.stay_id
	ORDER BY ig.stay_id
	)
	,ntrd5_comp
AS (
	SELECT d.stay_id
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_en_d5
			END cal_en_d5
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.cal_pn_d5
			END cal_pn_d5
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_en_d5
			END pr_en_d5
		,CASE 
			WHEN s1.stay_id IS NULL
				THEN 0
			ELSE s1.pr_pn_d5
			END pr_pn_d5
	FROM d
	LEFT JOIN ntrd5_comp_s1 s1 ON d.stay_id = s1.stay_id
	WHERE d.icu_outtime > DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
	ORDER BY d.stay_id
	)
	,
	--
	-- interval between icu_intime and first starttime of en
time_to_en
AS (
	SELECT d.stay_id
		,datetime_diff(MIN(ig.starttime), d.icu_intime, 'MINUTE') / 60 bfen_hour
	FROM d
	LEFT JOIN mimiciv_icu.ingredientevents ig ON d.stay_id = ig.stay_id
		AND ig.starttime >= d.icu_intime
	WHERE ig.itemid = 226221
	GROUP BY d.stay_id
		,d.icu_intime
	ORDER BY d.stay_id
	)
	,
	--
	-- sepsis3 in first 24h of icu stay
sepsis
AS (
	SELECT d.stay_id
		,CASE 
			WHEN s3.stay_id IS NOT NULL
				THEN 1
			ELSE 0
			END AS sepsis_flag
	FROM d
	LEFT JOIN sepsis3 s3 ON d.stay_id = s3.stay_id
		AND s3.suspected_infection_time < DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
		AND s3.sofa_time < DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	ORDER BY d.stay_id
	)
	,
	--
	-- mechanical ventilation in first 24h and duration in icu
vent_flag
AS (
	SELECT d.stay_id
		,MAX(CASE 
				WHEN v.ventilation_status IN (
						'Trach'
						,'InvasiveVent'
						,'NonInvasiveVent'
						)
					THEN 1
				ELSE 0
				END) AS mechvent_flag
	FROM d
	LEFT JOIN ventilation v ON v.stay_id = d.stay_id
		AND v.endtime > d.icu_intime
		AND v.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,vent_du
AS (
	SELECT d.stay_id
		,SUM(CASE 
				WHEN v.ventilation_status IN (
						'Trach'
						,'InvasiveVent'
						,'NonInvasiveVent'
						)
					THEN (
							CASE 
								WHEN v.starttime < d.icu_intime
									AND v.endtime <= d.icu_outtime
									THEN datetime_diff(v.endtime, d.icu_intime, 'MINUTE')
								WHEN v.starttime >= d.icu_intime
									AND v.endtime <= d.icu_outtime
									THEN datetime_diff(v.endtime, v.starttime, 'MINUTE')
								WHEN v.starttime >= d.icu_intime
									AND v.endtime > d.icu_outtime
									THEN datetime_diff(d.icu_outtime, v.starttime, 'MINUTE')
								WHEN v.starttime < d.icu_intime
									AND v.endtime > d.icu_outtime
									THEN datetime_diff(d.icu_outtime, d.icu_intime, 'MINUTE')
								END
							)
				ELSE NULL
				END) / 60 AS vent_hour
	FROM d
	LEFT JOIN ventilation v ON v.stay_id = d.stay_id
		AND v.starttime < d.icu_outtime
		AND v.endtime > d.icu_intime
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- crrt in first 24h of icu stay
	-- we hypothesize that charttime was at most one hour later than actual starttime
crrt
AS (
	SELECT d.stay_id
		,MAX(CASE 
				WHEN cr.stay_id IS NOT NULL
					THEN 1
				ELSE 0
				END) AS crrt_flag
	FROM d
	LEFT JOIN crrt cr ON cr.stay_id = d.stay_id
		AND cr.charttime >= d.icu_intime
		AND cr.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '25' HOUR)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- use of vasopressors in first 24h of icu stay
vasopressor
AS (
	SELECT d.stay_id
		,MAX(CASE 
				WHEN va.stay_id IS NOT NULL
					THEN 1
				ELSE 0
				END) AS vaso_flag
	FROM d
	LEFT JOIN vasoactive_agent va ON d.stay_id = va.stay_id
		AND va.endtime > d.icu_intime
		AND va.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- infection during icu stay
infection
AS (
	SELECT d.hadm_id
		,d.stay_id
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen IN (
						'BLOOD CULTURE'
						,'BLOOD CULTURE - NEONATE'
						,'BLOOD CULTURE ( MYCO/F LYTIC BOTTLE)'
						,'BLOOD CULTURE (POST-MORTEM)'
						)
					THEN 1
				ELSE 0
				END) AS blood_flag
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
				END) AS resp_flag
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen = 'PLEURAL FLUID'
					THEN 1
				ELSE 0
				END) AS chest_flag
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen IN (
						'URINE'
						,'URINE,KIDNEY'
						,'URINE,SUPRAPUBIC ASPIRATE'
						)
					THEN 1
				ELSE 0
				END) AS ut_flag
		,MAX(CASE 
				WHEN soi.positive_culture = 1
					AND soi.specimen = 'FLUID WOUND'
					THEN 1
				ELSE 0
				END) AS wound_flag
	FROM d
	LEFT JOIN suspicion_of_infection soi ON d.hadm_id = soi.hadm_id
		AND soi.culture_time >= d.icu_intime
		AND soi.culture_time <= d.icu_outtime
	GROUP BY d.hadm_id
		,d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- average blood glucose (mg/dL) each day from day 1 to 7 in icu
glu
AS (
	SELECT d.stay_id
		,AVG(CASE 
				WHEN chem.charttime >= d.icu_intime
					AND chem.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
					THEN chem.glucose
				ELSE NULL
				END) AS glu_d1
		,AVG(CASE 
				WHEN chem.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
					AND chem.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
					THEN chem.glucose
				ELSE NULL
				END) AS glu_d2
		,AVG(CASE 
				WHEN chem.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
					AND chem.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
					THEN chem.glucose
				ELSE NULL
				END) AS glu_d3
		,AVG(CASE 
				WHEN chem.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
					AND chem.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
					THEN chem.glucose
				ELSE NULL
				END) AS glu_d4
		,AVG(CASE 
				WHEN chem.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
					AND chem.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
					THEN chem.glucose
				ELSE NULL
				END) AS glu_d5
		,AVG(CASE 
				WHEN chem.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
					AND chem.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
					THEN chem.glucose
				ELSE NULL
				END) AS glu_d6
		,AVG(CASE 
				WHEN chem.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
					AND chem.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '7' DAY)
					THEN chem.glucose
				ELSE NULL
				END) AS glu_d7
	FROM d
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
	GROUP BY d.subject_id
		,d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- daily use of insulin (units) from day 1 to 7 in icu
ins
AS (
	SELECT d.stay_id
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
					AND ip.starttime >= d.icu_intime
					AND ip.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
					THEN ip.amount
				ELSE 0
				END) AS ins_d1
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
					AND ip.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
					AND ip.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
					THEN ip.amount
				ELSE 0
				END) AS ins_d2
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
					AND ip.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
					AND ip.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
					THEN ip.amount
				ELSE 0
				END) AS ins_d3
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
					AND ip.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
					AND ip.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
					THEN ip.amount
				ELSE 0
				END) AS ins_d4
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
					AND ip.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
					AND ip.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
					THEN ip.amount
				ELSE 0
				END) AS ins_d5
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
					AND ip.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
					AND ip.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
					THEN ip.amount
				ELSE 0
				END) AS ins_d6
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
					AND ip.starttime >= DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
					AND ip.starttime < DATETIME_ADD(d.icu_intime, INTERVAL '7' DAY)
					THEN ip.amount
				ELSE 0
				END) AS ins_d7
	FROM d
	LEFT JOIN mimiciv_icu.inputevents ip ON d.stay_id = ip.stay_id
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- total tube feeding (TF) residual i.e., gastric residual volume (GRV) (mL) each day from day 1 to 7 in icu
	-- we hypothesize that charttime was at most one hour later than actual starttime
grv
AS (
	SELECT d.stay_id
		,SUM(CASE 
				WHEN op.itemid IN (
						227510
						,227511
						)
					AND op.charttime > d.icu_intime
					AND op.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '25' HOUR)
					THEN op.value
				ELSE 0
				END) AS grv_d1
		,SUM(CASE 
				WHEN op.itemid IN (
						227510
						,227511
						)
					AND op.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '25' HOUR)
					AND op.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '49' HOUR)
					THEN op.value
				ELSE 0
				END) AS grv_d2
		,SUM(CASE 
				WHEN op.itemid IN (
						227510
						,227511
						)
					AND op.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '49' HOUR)
					AND op.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '73' HOUR)
					THEN op.value
				ELSE 0
				END) AS grv_d3
		,SUM(CASE 
				WHEN op.itemid IN (
						227510
						,227511
						)
					AND op.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '73' HOUR)
					AND op.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '97' HOUR)
					THEN op.value
				ELSE 0
				END) AS grv_d4
		,SUM(CASE 
				WHEN op.itemid IN (
						227510
						,227511
						)
					AND op.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '97' HOUR)
					AND op.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '121' HOUR)
					THEN op.value
				ELSE 0
				END) AS grv_d5
		,SUM(CASE 
				WHEN op.itemid IN (
						227510
						,227511
						)
					AND op.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '121' HOUR)
					AND op.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '145' HOUR)
					THEN op.value
				ELSE 0
				END) AS grv_d6
		,SUM(CASE 
				WHEN op.itemid IN (
						227510
						,227511
						)
					AND op.charttime >= DATETIME_ADD(d.icu_intime, INTERVAL '145' HOUR)
					AND op.charttime < DATETIME_ADD(d.icu_intime, INTERVAL '169' HOUR)
					THEN op.value
				ELSE 0
				END) AS grv_d7
	FROM d
	LEFT JOIN mimiciv_icu.outputevents op ON d.stay_id = op.stay_id
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- average lab results (WBC, neutrophil, lymphocyte, albumin, urea nitrogen, creatinine and total cholesterol) in first 24h of icu stay
	-- average lab results (WBC, lymphocyte, and albumin) each day from day 1 to 7 in icu
lab_d1
AS (
	SELECT d.stay_id
		,AVG(bd.wbc) wbc_avg_d1
		,AVG(bd.neutrophils_abs) neutrophil_avg_d1
		,AVG(bd.lymphocytes_abs) lymphocyte_avg_d1
		,AVG(chem.albumin) albumin_avg_d1
		,AVG(chem.creatinine) creatinine_avg_d1
		,AVG(chem.bun) bun_avg_d1
	FROM d
	LEFT JOIN blood_differential bd ON d.subject_id = bd.subject_id
		AND bd.charttime >= d.icu_intime
		AND bd.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
		AND chem.charttime >= d.icu_intime
		AND chem.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,lab_d2
AS (
	SELECT d.stay_id
		,AVG(bd.wbc) wbc_avg_d2
		,AVG(bd.lymphocytes_abs) lymphocyte_avg_d2
		,AVG(chem.albumin) albumin_avg_d2
	FROM d
	LEFT JOIN blood_differential bd ON d.subject_id = bd.subject_id
		AND bd.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
		AND bd.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
		AND chem.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '1' DAY)
		AND chem.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,lab_d3
AS (
	SELECT d.stay_id
		,AVG(bd.wbc) wbc_avg_d3
		,AVG(bd.lymphocytes_abs) lymphocyte_avg_d3
		,AVG(chem.albumin) albumin_avg_d3
	FROM d
	LEFT JOIN blood_differential bd ON d.subject_id = bd.subject_id
		AND bd.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
		AND bd.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
		AND chem.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '2' DAY)
		AND chem.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,lab_d4
AS (
	SELECT d.stay_id
		,AVG(bd.wbc) wbc_avg_d4
		,AVG(bd.lymphocytes_abs) lymphocyte_avg_d4
		,AVG(chem.albumin) albumin_avg_d4
	FROM d
	LEFT JOIN blood_differential bd ON d.subject_id = bd.subject_id
		AND bd.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
		AND bd.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
		AND chem.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '3' DAY)
		AND chem.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,lab_d5
AS (
	SELECT d.stay_id
		,AVG(bd.wbc) wbc_avg_d5
		,AVG(bd.lymphocytes_abs) lymphocyte_avg_d5
		,AVG(chem.albumin) albumin_avg_d5
	FROM d
	LEFT JOIN blood_differential bd ON d.subject_id = bd.subject_id
		AND bd.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
		AND bd.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
		AND chem.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '4' DAY)
		AND chem.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,lab_d6
AS (
	SELECT d.stay_id
		,AVG(bd.wbc) wbc_avg_d6
		,AVG(bd.lymphocytes_abs) lymphocyte_avg_d6
		,AVG(chem.albumin) albumin_avg_d6
	FROM d
	LEFT JOIN blood_differential bd ON d.subject_id = bd.subject_id
		AND bd.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
		AND bd.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
		AND chem.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '5' DAY)
		AND chem.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,lab_d7
AS (
	SELECT d.stay_id
		,AVG(bd.wbc) wbc_avg_d7
		,AVG(bd.lymphocytes_abs) lymphocyte_avg_d7
		,AVG(chem.albumin) albumin_avg_d7
	FROM d
	LEFT JOIN blood_differential bd ON d.subject_id = bd.subject_id
		AND bd.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
		AND bd.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '7' DAY)
	LEFT JOIN chemistry chem ON d.subject_id = chem.subject_id
		AND chem.charttime > DATETIME_ADD(d.icu_intime, INTERVAL '6' DAY)
		AND chem.charttime <= DATETIME_ADD(d.icu_intime, INTERVAL '7' DAY)
	GROUP BY d.stay_id
	ORDER BY d.stay_id
	)
	,
	--
	-- icu, hospital mortality
mor_flag
AS (
	SELECT d.stay_id
		,CASE 
			WHEN adm.deathtime BETWEEN d.icu_intime
					AND d.icu_outtime
				THEN 1
			WHEN adm.deathtime <= d.icu_intime -- sometimes there are typographical errors in the death date
				THEN 1
			WHEN adm.dischtime <= d.icu_outtime
				AND adm.discharge_location = 'DIED'
				THEN 1
			ELSE 0
			END icustay_expire_flag
		,adm.hospital_expire_flag
	FROM d
	LEFT JOIN mimiciv_hosp.admissions adm ON d.hadm_id = adm.hadm_id
	ORDER BY d.stay_id
	)
	,
	--
	-- 28 day and 1 year survival data
sur_time -- survival time (day)
AS (
	SELECT d.stay_id
		,CASE 
			WHEN s1.dod IS NOT NULL
				THEN datetime_diff(s1.dod, d.icu_intime, 'HOUR') / 24
			ELSE 365 + datetime_diff(s1.dtime, d.icu_intime, 'HOUR') / 24
			END sur_day
	FROM d
	LEFT JOIN (
		SELECT ad.subject_id
			,MAX(ad.dischtime) dtime
			,MAX(pa.dod) dod
		FROM mimiciv_hosp.admissions ad
		LEFT JOIN mimiciv_hosp.patients pa ON ad.subject_id = pa.subject_id
		GROUP BY ad.subject_id
		) s1 ON d.subject_id = s1.subject_id
	)
	,sur_flag -- survival status at 28 day and 1 year
AS (
	SELECT st.stay_id
		,st.sur_day
		,CASE 
			WHEN st.sur_day <= 28
				THEN 1
			ELSE 0
			END sur_flag_28d
		,CASE 
			WHEN st.sur_day <= 365
				THEN 1
			ELSE 0
			END sur_flag_1y
	FROM sur_time st
	)
	--
	-- BMI of admission to ICU
	,bmi_admit
AS (
	SELECT d.stay_id
		,MAX(10000 * weight.weight / (height.height * height.height)) bmi_admit
	FROM d
	LEFT JOIN height ON d.stay_id = height.stay_id
	LEFT JOIN weight_durations weight ON d.stay_id = weight.stay_id
	WHERE weight.weight_type = 'admit'
	GROUP BY d.stay_id
	)
	--
	-- BMI of discharge from ICU
	,wt_s1
AS (
	SELECT stay_id
		,weight
		,ROW_NUMBER() OVER (
			PARTITION BY stay_id ORDER BY starttime DESC
			) rn
	FROM weight_durations
	WHERE weight_type = 'daily'
	)
	,bmi_disch
AS (
	SELECT d.stay_id
		,MAX(10000 * s1.weight / (height.height * height.height)) bmi_disch
	FROM d
	LEFT JOIN height ON d.stay_id = height.stay_id
	LEFT JOIN wt_s1 s1 ON d.stay_id = s1.stay_id
	WHERE s1.rn = 1
	GROUP BY d.stay_id
	)
	--
	-- sofa in first 24h of icu stay
	,sofa_max
AS (
	SELECT sofa.stay_id
		,MAX(sofa.hr) hr_max
	FROM sofa
	GROUP BY sofa.stay_id
	)
	,sofa_s1
AS (
	SELECT sofa.stay_id
		,CASE 
			WHEN sm.hr_max < 23
				AND sofa.hr = sm.hr_max
				THEN sofa.sofa_24hours
			WHEN sm.hr_max >= 23
				AND sofa.hr = 23
				THEN sofa.sofa_24hours
			ELSE NULL
			END sofa_24h
	FROM sofa
	INNER JOIN sofa_max sm ON sofa.stay_id = sm.stay_id
	)
	,sofa
AS (
	SELECT s1.*
	FROM sofa_s1 s1
	WHERE s1.sofa_24h IS NOT NULL
	)
SELECT d.stay_id
	,age.age
	,d.gender
	,d.race
	,bmi_admit.bmi_admit
	,bmi_disch.bmi_disch
	,charlson.charlson_comorbidity_index
	,d.careunit
	,mnutric.mnutric
	,sofa.sofa_24h
	,apacheii.apacheii
	,vasopressor.vaso_flag
	,sepsis.sepsis_flag
	,vent_flag.mechvent_flag
	,crrt.crrt_flag
	,time_to_en.bfen_hour
	,lab_d1.wbc_avg_d1
	,lab_d1.neutrophil_avg_d1
	,lab_d1.lymphocyte_avg_d1
	,lab_d1.albumin_avg_d1
	,lab_d1.bun_avg_d1
	,lab_d1.creatinine_avg_d1
	,lab_d2.wbc_avg_d2
	,lab_d2.lymphocyte_avg_d2
	,lab_d2.albumin_avg_d2
	,lab_d3.wbc_avg_d3
	,lab_d3.lymphocyte_avg_d3
	,lab_d3.albumin_avg_d3
	,lab_d4.wbc_avg_d4
	,lab_d4.lymphocyte_avg_d4
	,lab_d4.albumin_avg_d4
	,lab_d5.wbc_avg_d5
	,lab_d5.lymphocyte_avg_d5
	,lab_d5.albumin_avg_d5
	,lab_d6.wbc_avg_d6
	,lab_d6.lymphocyte_avg_d6
	,lab_d6.albumin_avg_d6
	,lab_d7.wbc_avg_d7
	,lab_d7.lymphocyte_avg_d7
	,lab_d7.albumin_avg_d7
	,ntrd1.en_d1
	,ntrd2.en_d2
	,ntrd3.en_d3
	,ntrd4.en_d4
	,ntrd5.en_d5
	,ntrd1.pn_d1
	,ntrd2.pn_d2
	,ntrd3.pn_d3
	,ntrd4.pn_d4
	,ntrd5.pn_d5
	,ntrd1.cal_tot_d1
	,ntrd2.cal_tot_d2
	,ntrd3.cal_tot_d3
	,ntrd4.cal_tot_d4
	,ntrd5.cal_tot_d5
	,ntrd1.pr_tot_d1
	,ntrd2.pr_tot_d2
	,ntrd3.pr_tot_d3
	,ntrd4.pr_tot_d4
	,ntrd5.pr_tot_d5
	,ntrd1_comp.cal_en_d1
	,ntrd2_comp.cal_en_d2
	,ntrd3_comp.cal_en_d3
	,ntrd4_comp.cal_en_d4
	,ntrd5_comp.cal_en_d5
	,ntrd1_comp.cal_pn_d1
	,ntrd2_comp.cal_pn_d2
	,ntrd3_comp.cal_pn_d3
	,ntrd4_comp.cal_pn_d4
	,ntrd5_comp.cal_pn_d5
	,ntrd1_comp.pr_en_d1
	,ntrd2_comp.pr_en_d2
	,ntrd3_comp.pr_en_d3
	,ntrd4_comp.pr_en_d4
	,ntrd5_comp.pr_en_d5
	,ntrd1_comp.pr_pn_d1
	,ntrd2_comp.pr_pn_d2
	,ntrd3_comp.pr_pn_d3
	,ntrd4_comp.pr_pn_d4
	,ntrd5_comp.pr_pn_d5
	,d.los_hospital
	,d.los_icu
	,vent_du.vent_hour
	,infection.blood_flag
	,infection.resp_flag
	,infection.chest_flag
	,infection.ut_flag
	,infection.wound_flag
	,grv.grv_d1
	,grv.grv_d2
	,grv.grv_d3
	,grv.grv_d4
	,grv.grv_d5
	,grv.grv_d6
	,grv.grv_d7
	,glu.glu_d1
	,glu.glu_d2
	,glu.glu_d3
	,glu.glu_d4
	,glu.glu_d5
	,glu.glu_d6
	,glu.glu_d7
	,ins.ins_d1
	,ins.ins_d2
	,ins.ins_d3
	,ins.ins_d4
	,ins.ins_d5
	,ins.ins_d6
	,ins.ins_d7
	,mor_flag.icustay_expire_flag
	,mor_flag.hospital_expire_flag
	,sur_time.sur_day
	,sur_flag.sur_flag_28d
	,sur_flag.sur_flag_1y
FROM d
LEFT JOIN age ON d.hadm_id = age.hadm_id
LEFT JOIN charlson ON d.hadm_id = charlson.hadm_id
LEFT JOIN bmi_admit ON d.stay_id = bmi_admit.stay_id
LEFT JOIN bmi_disch ON d.stay_id = bmi_disch.stay_id
LEFT JOIN mnutric ON d.stay_id = mnutric.stay_id
LEFT JOIN sofa ON d.stay_id = sofa.stay_id
LEFT JOIN apacheii ON d.stay_id = apacheii.stay_id
LEFT JOIN vasopressor ON d.stay_id = vasopressor.stay_id
LEFT JOIN sepsis ON d.stay_id = sepsis.stay_id
LEFT JOIN vent_flag ON d.stay_id = vent_flag.stay_id
LEFT JOIN crrt ON d.stay_id = crrt.stay_id
LEFT JOIN time_to_en ON d.stay_id = time_to_en.stay_id
LEFT JOIN lab_d1 ON d.stay_id = lab_d1.stay_id
LEFT JOIN lab_d2 ON d.stay_id = lab_d2.stay_id
LEFT JOIN lab_d3 ON d.stay_id = lab_d3.stay_id
LEFT JOIN lab_d4 ON d.stay_id = lab_d4.stay_id
LEFT JOIN lab_d5 ON d.stay_id = lab_d5.stay_id
LEFT JOIN lab_d6 ON d.stay_id = lab_d6.stay_id
LEFT JOIN lab_d7 ON d.stay_id = lab_d7.stay_id
LEFT JOIN ntrd1 ON d.stay_id = ntrd1.stay_id
LEFT JOIN ntrd2 ON d.stay_id = ntrd2.stay_id
LEFT JOIN ntrd3 ON d.stay_id = ntrd3.stay_id
LEFT JOIN ntrd4 ON d.stay_id = ntrd4.stay_id
LEFT JOIN ntrd5 ON d.stay_id = ntrd5.stay_id
LEFT JOIN ntrd1_comp ON d.stay_id = ntrd1_comp.stay_id
LEFT JOIN ntrd2_comp ON d.stay_id = ntrd2_comp.stay_id
LEFT JOIN ntrd3_comp ON d.stay_id = ntrd3_comp.stay_id
LEFT JOIN ntrd4_comp ON d.stay_id = ntrd4_comp.stay_id
LEFT JOIN ntrd5_comp ON d.stay_id = ntrd5_comp.stay_id
LEFT JOIN vent_du ON d.stay_id = vent_du.stay_id
LEFT JOIN infection ON d.stay_id = infection.stay_id
LEFT JOIN grv ON d.stay_id = grv.stay_id
LEFT JOIN glu ON d.stay_id = glu.stay_id
LEFT JOIN ins ON d.stay_id = ins.stay_id
LEFT JOIN mor_flag ON d.stay_id = mor_flag.stay_id
LEFT JOIN sur_time ON d.stay_id = sur_time.stay_id
LEFT JOIN sur_flag ON d.stay_id = sur_flag.stay_id