CREATE MATERIALIZED VIEW PUBLIC.apacheii AS
	-- references: 
	-- KNAUS, W. A., DRAPER, E. A., WAGNER, D. P., & ZIMMERMAN, J. E. (1985). APACHE II: A severity of disease classification system. Critical Care Medicine, 13(10), 818-29.
	-- https://github.com/MIT-LCP/mimic-code/blob/main/mimic-iv/concepts_postgres/score/sapsii.sql
	-- https://github.com/MIT-LCP/mimic-code/blob/main/mimic-iv/concepts_postgres/score/apsiii.sql
	WITH co AS (
			SELECT subject_id
				,hadm_id
				,stay_id
				,intime AS starttime
				,DATETIME_ADD(intime, INTERVAL '24' HOUR) AS endtime
			FROM mimiciv_icu.icustays
			)
		,vital AS (
			SELECT co.stay_id
				,MIN(vital.heart_rate) AS heartrate_min
				,MAX(vital.heart_rate) AS heartrate_max
				,MIN(vital.mbp) AS mbp_min
				,MAX(vital.mbp) AS mbp_max
				,MIN(vital.temperature) AS temp_min
				,MAX(vital.temperature) AS temp_max
				,MIN(vital.resp_rate) AS resp_min
				,MAX(vital.resp_rate) AS resp_max
			FROM co
			LEFT JOIN vitalsign vital ON co.subject_id = vital.subject_id
				AND co.starttime < vital.charttime
				AND co.endtime >= vital.charttime
			GROUP BY co.stay_id
			)
		,labs AS (
			SELECT co.stay_id
				,MIN(labs.creatinine) AS cr_min
				,MAX(labs.creatinine) AS cr_max
				,MIN(labs.potassium) AS potassium_min
				,MAX(labs.potassium) AS potassium_max
				,MIN(labs.sodium) AS sodium_min
				,MAX(labs.sodium) AS sodium_max
			FROM co
			LEFT JOIN chemistry labs ON co.subject_id = labs.subject_id
				AND co.starttime < labs.charttime
				AND co.endtime >= labs.charttime
			GROUP BY co.stay_id
			)
		,cbc AS (
			SELECT co.stay_id
				,MIN(cbc.wbc) AS wbc_min
				,MAX(cbc.wbc) AS wbc_max
				,MIN(cbc.hematocrit) AS hct_min
				,MAX(cbc.hematocrit) AS hct_max
			FROM co
			LEFT JOIN complete_blood_count cbc ON co.subject_id = cbc.subject_id
				AND co.starttime < cbc.charttime
				AND co.endtime >= cbc.charttime
			GROUP BY co.stay_id
			)
		,gcs AS (
			SELECT co.stay_id
				,MIN(gcs.gcs) AS gcs_min
			FROM co
			LEFT JOIN gcs ON co.stay_id = gcs.stay_id
				AND co.starttime < gcs.charttime
				AND co.endtime >= gcs.charttime
			GROUP BY co.stay_id
			)
		,ph AS (
			SELECT co.stay_id
				,MIN(bg.ph) AS ph_min
				,MAX(bg.ph) AS ph_max
			FROM co
			LEFT JOIN bg ON co.subject_id = bg.subject_id
				AND co.starttime < bg.charttime
				AND co.endtime >= bg.charttime
			WHERE bg.specimen_pred = 'ART.'
			GROUP BY co.stay_id
			)
		,pao2 AS (
			SELECT co.stay_id
				,MIN(bg.po2) AS pao2_min
			FROM co
			LEFT JOIN bg ON co.subject_id = bg.subject_id
				AND co.starttime < bg.charttime
				AND co.endtime >= bg.charttime
			WHERE bg.specimen_pred = 'ART.'
				AND COALESCE(fio2, fio2_chartevents, 21) < 50 -- fio2 < 50, or if no fio2, assume room air
			GROUP BY co.stay_id
			)
		,aado2 AS (
			SELECT co.stay_id
				,GREATEST(MAX(bg.aado2), MAX(bg.aado2_calc)) AS aado2_max
			FROM co
			LEFT JOIN bg ON co.subject_id = bg.subject_id
				AND co.starttime < bg.charttime
				AND co.endtime >= bg.charttime
			WHERE bg.specimen_pred = 'ART.'
				AND COALESCE(fio2, fio2_chartevents) >= 50
			GROUP BY co.stay_id
			)
		-- extract a flag for surgical service
		-- this combined with "elective" from admissions table
		-- defines elective/non-elective surgery
		,surgflag AS (
			SELECT adm.hadm_id
				,MAX(CASE 
						WHEN LOWER(curr_service) LIKE '%surg%'
							OR curr_service = 'ORTHO'
							THEN 1
						ELSE 0
						END) AS surgical
			FROM mimiciv_hosp.admissions adm
			LEFT JOIN mimiciv_hosp.services se ON adm.hadm_id = se.hadm_id
			GROUP BY adm.hadm_id
			)
		-- 'organ insufficiency or immuno-compromised state must have been evident prior to this hospital admission'
		-- here we suppose organ insufficiency or immuno-compromised state during last hospitalization was the same as this hospitalization
		,comorb AS (
			SELECT hadm_id
				,MAX(CASE 
						WHEN icd_version = 9
							AND icd_code IN (
								'5712'
								,'5715'
								)
							THEN 1
						WHEN icd_version = 10
							AND icd_code IN (
								'K703'
								,'K7030'
								,'K7031'
								,'K717'
								,'K74'
								,'K746'
								,'K7460'
								,'K7469'
								,'P7881'
								)
							THEN 1
						ELSE 0
						END) AS cirrhosis -- cirrhosis of liver
				,MAX(CASE 
						WHEN icd_version = 9
							AND icd_code = '5723'
							THEN 1
						WHEN icd_version = 10
							AND icd_code = 'K766'
							THEN 1
						ELSE 0
						END) AS por_ht -- portal hypertension
				,MAX(CASE 
						WHEN icd_version = 9
							AND icd_code IN (
								'0700'
								,'07020'
								,'07021'
								,'07022'
								,'07023'
								,'07041'
								,'07042'
								,'07043'
								,'07044'
								,'07049'
								,'0706'
								,'07071'
								,'5722'
								)
							THEN 1
						WHEN icd_version = 10
							AND (
								icd_code IN (
									'B150'
									,'B160'
									,'B162'
									,'B1711'
									,'B190'
									,'B1911'
									,'B1921'
									,'K704'
									,'K7040'
									,'K7041'
									,'K7111'
									,'K9182'
									,'T8642'
									)
								OR icd_code LIKE 'K72%'
								)
							THEN 1
						ELSE 0
						END) AS hf -- hepatic failure/encephalopathy/coma
				,MAX(CASE 
						WHEN icd_version = 9
							AND icd_code IN (
								'4160'
								,'51853'
								,'51883'
								,'51884'
								)
							THEN 1
						WHEN icd_version = 10
							AND (
								icd_code LIKE 'J44%'
								OR icd_code LIKE 'I272%'
								OR icd_code IN (
									'I270'
									,'D751'
									,'J95822'
									,'J961'
									,'J9610'
									,'J9611'
									,'J9612'
									,'J962'
									,'J9620'
									,'J9621'
									,'J9622'
									)
								)
							THEN 1
						ELSE 0
						END) AS respiratory -- copd, chronic respiratory failure, secondary polycythemia, pulmonary hypertension
				,MAX(CASE 
						WHEN icd_version = 9
							AND icd_code IN (
								'V451'
								,'V4511'
								)
							THEN 1
						WHEN icd_version = 10
							AND icd_code = 'Z992'
							THEN 1
						ELSE 0
						END) AS cd -- chronic dialysis		
				,MAX(CASE 
						WHEN icd_version = 9
							AND (
								icd_code IN (
									'V8746'
									,'V581'
									,'V5811'
									,'V8741'
									,'V580'
									,'V5865'
									)
								OR icd_code BETWEEN '27900'
									AND '27910'
								)
							THEN 1
						WHEN icd_version = 10
							AND (
								icd_code IN (
									'Z9225'
									,'Z511'
									,'Z5111'
									,'Z9221'
									,'Z510'
									,'Z795'
									,'Z7951'
									,'Z7952'
									,'F553'
									)
								OR SUBSTR(icd_code, 1, 3) BETWEEN 'D80'
									AND 'D84'
								)
							THEN 1
						ELSE 0
						END) AS immune -- immunodeficiency, immunosuppression therapy, chemotherapy, radiotherapy, long term steroids		 
				,MAX(CASE 
						WHEN icd_version = 9
							AND SUBSTR(icd_code, 1, 3) BETWEEN '042'
								AND '044'
							THEN 1
						WHEN icd_version = 10
							AND SUBSTR(icd_code, 1, 3) BETWEEN 'B20'
								AND 'B22'
							THEN 1
						WHEN icd_version = 10
							AND SUBSTR(icd_code, 1, 3) = 'B24'
							THEN 1
						ELSE 0
						END) AS aids -- HIV and AIDS
				,MAX(CASE 
						WHEN icd_version = 9
							THEN CASE 
									-- lymphoma
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20000'
											AND '20238'
										THEN 1
											-- leukemia
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20240'
											AND '20248'
										THEN 1
											-- lymphoma
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20250'
											AND '20302'
										THEN 1
											-- leukemia
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20310'
											AND '20312'
										THEN 1
											-- lymphoma
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20302'
											AND '20382'
										THEN 1
											-- chronic leukemia
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20400'
											AND '20522'
										THEN 1
											-- other myeloid leukemia
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20580'
											AND '20702'
										THEN 1
											-- other myeloid leukemia
									WHEN SUBSTR(icd_code, 1, 5) BETWEEN '20720'
											AND '20892'
										THEN 1
											-- lymphoma
									WHEN SUBSTR(icd_code, 1, 4) IN (
											'2386'
											,'2733'
											)
										THEN 1
									ELSE 0
									END
						WHEN icd_version = 10
							AND SUBSTR(icd_code, 1, 3) BETWEEN 'C81'
								AND 'C96'
							THEN 1
						ELSE 0
						END) AS hem
				,MAX(CASE 
						WHEN icd_version = 9
							AND (
								icd_code LIKE '584%'
								OR icd_code LIKE '6693%'
								)
							THEN 1
						WHEN icd_version = 10
							AND (
								icd_code = 'O904'
								OR icd_code LIKE 'N17%'
								)
							THEN 1
						ELSE 0
						END) AS arf -- acute renal failure	
			FROM mimiciv_hosp.diagnoses_icd
			GROUP BY hadm_id
			)
		,cohort AS (
			SELECT ie.subject_id
				,ie.hadm_id
				,ie.stay_id
				,va.age
				,vital.heartrate_min
				,vital.heartrate_max
				,vital.mbp_min
				,vital.mbp_max
				,vital.temp_min
				,vital.temp_max
				,vital.resp_min
				,vital.resp_max
				,labs.cr_min
				,labs.cr_max
				,labs.potassium_min
				,labs.potassium_max
				,labs.sodium_min
				,labs.sodium_max
				,cbc.wbc_min
				,cbc.wbc_max
				,cbc.hct_min
				,cbc.hct_max
				,gcs.gcs_min AS mingcs
				,ph.ph_min
				,ph.ph_max
				,pao2.pao2_min
				,aado2.aado2_max
				,CASE 
					WHEN adm.admission_type = 'ELECTIVE'
						AND sf.surgical = 1
						THEN 'ScheduledSurgical'
					WHEN adm.admission_type != 'ELECTIVE'
						AND sf.surgical = 1
						THEN 'UnscheduledSurgical'
					ELSE 'Medical'
					END AS admissiontype
				,GREATEST(com.cirrhosis, com.por_ht, com.hf, com.respiratory, com.cd, com.immune, com.aids, com.hem) AS chronic_disease
				,com.arf
			FROM mimiciv_icu.icustays ie
			INNER JOIN mimiciv_hosp.admissions adm ON ie.hadm_id = adm.hadm_id
			LEFT JOIN age va ON ie.hadm_id = va.hadm_id
			LEFT JOIN vital ON ie.stay_id = vital.stay_id
			LEFT JOIN labs ON ie.stay_id = labs.stay_id
			LEFT JOIN cbc ON ie.stay_id = cbc.stay_id
			LEFT JOIN gcs gcs ON ie.stay_id = gcs.stay_id
			LEFT JOIN ph ON ie.stay_id = ph.stay_id
			LEFT JOIN pao2 ON ie.stay_id = pao2.stay_id
			LEFT JOIN aado2 ON ie.stay_id = aado2.stay_id
			LEFT JOIN surgflag sf ON ie.hadm_id = sf.hadm_id
			LEFT JOIN comorb com ON ie.hadm_id = com.hadm_id
			)
		-- first, we calculate the score for the minimum values
		,score_min AS (
			SELECT cohort.subject_id
				,cohort.hadm_id
				,cohort.stay_id
				,CASE 
					WHEN heartrate_min IS NULL
						THEN NULL
					WHEN heartrate_min < 40
						THEN 4
					WHEN heartrate_min < 55
						THEN 3
					WHEN heartrate_min < 70
						THEN 2
					WHEN heartrate_min < 110
						THEN 0
					WHEN heartrate_min < 140
						THEN 2
					WHEN heartrate_min < 180
						THEN 3
					WHEN heartrate_min >= 180
						THEN 4
					END AS hr_score
				,CASE 
					WHEN mbp_min IS NULL
						THEN NULL
					WHEN mbp_min < 50
						THEN 4
					WHEN mbp_min < 70
						THEN 2
					WHEN mbp_min < 110
						THEN 0
					WHEN mbp_min < 130
						THEN 2
					WHEN mbp_min < 160
						THEN 3
					WHEN mbp_min >= 160
						THEN 4
					END AS mbp_score
				,CASE 
					WHEN temp_min IS NULL
						THEN NULL
					WHEN temp_min < 30.0
						THEN 4
					WHEN temp_min < 32.0
						THEN 3
					WHEN temp_min < 34.0
						THEN 2
					WHEN temp_min < 36.0
						THEN 1
					WHEN temp_min < 38.5
						THEN 0
					WHEN temp_min < 39.0
						THEN 1
					WHEN temp_min < 41.0
						THEN 3
					WHEN temp_min >= 41.0
						THEN 4
					END AS temp_score
				,CASE 
					WHEN resp_min IS NULL
						THEN NULL
					WHEN resp_min < 6
						THEN 4
					WHEN resp_min < 10
						THEN 2
					WHEN resp_min < 12
						THEN 1
					WHEN resp_min < 25
						THEN 0
					WHEN resp_min < 35
						THEN 1
					WHEN resp_min < 50
						THEN 3
					WHEN resp_min >= 50
						THEN 4
					END AS resp_rate_score
				,CASE 
					WHEN cohort.arf = 1
						THEN CASE 
								WHEN cr_min IS NULL
									THEN NULL
								WHEN cr_min < 0.6
									THEN 4
								WHEN cr_min < 1.5
									THEN 0
								WHEN cr_min < 2.0
									THEN 4
								WHEN cr_min < 3.5
									THEN 6
								WHEN cr_min >= 3.5
									THEN 8
								END
					WHEN cohort.arf = 0
						THEN CASE 
								WHEN cr_min IS NULL
									THEN NULL
								WHEN cr_min < 0.6
									THEN 2
								WHEN cr_min < 1.5
									THEN 0
								WHEN cr_min < 2.0
									THEN 2
								WHEN cr_min < 3.5
									THEN 3
								WHEN cr_min >= 3.5
									THEN 4
								END
					END AS cr_score
				,CASE 
					WHEN potassium_min IS NULL
						THEN NULL
					WHEN potassium_min < 2.5
						THEN 4
					WHEN potassium_min < 3.0
						THEN 2
					WHEN potassium_min < 3.5
						THEN 1
					WHEN potassium_min < 5.5
						THEN 0
					WHEN potassium_min < 6.0
						THEN 1
					WHEN potassium_min < 7.0
						THEN 3
					WHEN potassium_min >= 7.0
						THEN 4
					END AS potassium_score
				,CASE 
					WHEN sodium_min IS NULL
						THEN NULL
					WHEN sodium_min < 111
						THEN 4
					WHEN sodium_min < 120
						THEN 3
					WHEN sodium_min < 130
						THEN 2
					WHEN sodium_min < 150
						THEN 0
					WHEN sodium_min < 155
						THEN 1
					WHEN sodium_min < 160
						THEN 2
					WHEN sodium_min < 180
						THEN 3
					WHEN sodium_min >= 180
						THEN 4
					END AS sodium_score
				,CASE 
					WHEN wbc_min IS NULL
						THEN NULL
					WHEN wbc_min < 1.0
						THEN 4
					WHEN wbc_min < 3.0
						THEN 2
					WHEN wbc_min < 15.0
						THEN 0
					WHEN wbc_min < 20.0
						THEN 1
					WHEN wbc_min < 40.0
						THEN 2
					WHEN wbc_min >= 40.0
						THEN 4
					END AS wbc_score
				,CASE 
					WHEN hct_min IS NULL
						THEN NULL
					WHEN hct_min < 20.0
						THEN 4
					WHEN hct_min < 30.0
						THEN 2
					WHEN hct_min < 46.0
						THEN 0
					WHEN hct_min < 50.0
						THEN 1
					WHEN hct_min < 60.0
						THEN 2
					WHEN hct_min >= 60.0
						THEN 4
					END AS hct_score
				,CASE 
					WHEN ph_min IS NULL
						THEN NULL
					WHEN ph_min < 7.15
						THEN 4
					WHEN ph_min < 7.25
						THEN 3
					WHEN ph_min < 7.33
						THEN 2
					WHEN ph_min < 7.50
						THEN 0
					WHEN ph_min < 7.60
						THEN 1
					WHEN ph_min < 7.70
						THEN 3
					WHEN ph_min >= 7.70
						THEN 4
					END AS ph_score
				,CASE 
					WHEN pao2_min IS NULL
						THEN NULL
					WHEN pao2_min < 55
						THEN 4
					WHEN pao2_min < 61
						THEN 3
					WHEN pao2_min <= 70
						THEN 1
					WHEN pao2_min > 70
						THEN 0
					END AS pao2_score
			FROM cohort
			)
		,score_max AS (
			SELECT cohort.subject_id
				,cohort.hadm_id
				,cohort.stay_id
				,CASE 
					WHEN heartrate_max IS NULL
						THEN NULL
					WHEN heartrate_max < 40
						THEN 4
					WHEN heartrate_max < 55
						THEN 3
					WHEN heartrate_max < 70
						THEN 2
					WHEN heartrate_max < 110
						THEN 0
					WHEN heartrate_max < 140
						THEN 2
					WHEN heartrate_max < 180
						THEN 3
					WHEN heartrate_max >= 180
						THEN 4
					END AS hr_score
				,CASE 
					WHEN mbp_max IS NULL
						THEN NULL
					WHEN mbp_max < 50
						THEN 4
					WHEN mbp_max < 70
						THEN 2
					WHEN mbp_max < 110
						THEN 0
					WHEN mbp_max < 130
						THEN 2
					WHEN mbp_max < 160
						THEN 3
					WHEN mbp_max >= 160
						THEN 4
					END AS mbp_score
				,CASE 
					WHEN temp_max IS NULL
						THEN NULL
					WHEN temp_max < 30.0
						THEN 4
					WHEN temp_max < 32.0
						THEN 3
					WHEN temp_max < 34.0
						THEN 2
					WHEN temp_max < 36.0
						THEN 1
					WHEN temp_max < 38.5
						THEN 0
					WHEN temp_max < 39.0
						THEN 1
					WHEN temp_max < 41.0
						THEN 3
					WHEN temp_max >= 41.0
						THEN 4
					END AS temp_score
				,CASE 
					WHEN resp_max IS NULL
						THEN NULL
					WHEN resp_max < 6
						THEN 4
					WHEN resp_max < 10
						THEN 2
					WHEN resp_max < 12
						THEN 1
					WHEN resp_max < 25
						THEN 0
					WHEN resp_max < 35
						THEN 1
					WHEN resp_max < 50
						THEN 3
					WHEN resp_max >= 50
						THEN 4
					END AS resp_rate_score
				,CASE 
					WHEN cohort.arf = 1
						THEN CASE 
								WHEN cr_max IS NULL
									THEN NULL
								WHEN cr_max < 0.6
									THEN 4
								WHEN cr_max < 1.5
									THEN 0
								WHEN cr_max < 2.0
									THEN 4
								WHEN cr_max < 3.5
									THEN 6
								WHEN cr_max >= 3.5
									THEN 8
								END
					WHEN cohort.arf = 0
						THEN CASE 
								WHEN cr_max IS NULL
									THEN NULL
								WHEN cr_max < 0.6
									THEN 2
								WHEN cr_max < 1.5
									THEN 0
								WHEN cr_max < 2.0
									THEN 2
								WHEN cr_max < 3.5
									THEN 3
								WHEN cr_max >= 3.5
									THEN 4
								END
					END AS cr_score
				,CASE 
					WHEN potassium_max IS NULL
						THEN NULL
					WHEN potassium_max < 2.5
						THEN 4
					WHEN potassium_max < 3.0
						THEN 2
					WHEN potassium_max < 3.5
						THEN 1
					WHEN potassium_max < 5.5
						THEN 0
					WHEN potassium_max < 6.0
						THEN 1
					WHEN potassium_max < 7.0
						THEN 3
					WHEN potassium_max >= 7.0
						THEN 4
					END AS potassium_score
				,CASE 
					WHEN sodium_max IS NULL
						THEN NULL
					WHEN sodium_max < 111
						THEN 4
					WHEN sodium_max < 120
						THEN 3
					WHEN sodium_max < 130
						THEN 2
					WHEN sodium_max < 150
						THEN 0
					WHEN sodium_max < 155
						THEN 1
					WHEN sodium_max < 160
						THEN 2
					WHEN sodium_max < 180
						THEN 3
					WHEN sodium_max >= 180
						THEN 4
					END AS sodium_score
				,CASE 
					WHEN wbc_max IS NULL
						THEN NULL
					WHEN wbc_max < 1.0
						THEN 4
					WHEN wbc_max < 3.0
						THEN 2
					WHEN wbc_max < 15.0
						THEN 0
					WHEN wbc_max < 20.0
						THEN 1
					WHEN wbc_max < 40.0
						THEN 2
					WHEN wbc_max >= 40.0
						THEN 4
					END AS wbc_score
				,CASE 
					WHEN hct_max IS NULL
						THEN NULL
					WHEN hct_max < 20.0
						THEN 4
					WHEN hct_max < 30.0
						THEN 2
					WHEN hct_max < 46.0
						THEN 0
					WHEN hct_max < 50.0
						THEN 1
					WHEN hct_max < 60.0
						THEN 2
					WHEN hct_max >= 60.0
						THEN 4
					END AS hct_score
				,CASE 
					WHEN ph_max IS NULL
						THEN NULL
					WHEN ph_max < 7.15
						THEN 4
					WHEN ph_max < 7.25
						THEN 3
					WHEN ph_max < 7.33
						THEN 2
					WHEN ph_max < 7.50
						THEN 0
					WHEN ph_max < 7.60
						THEN 1
					WHEN ph_max < 7.70
						THEN 3
					WHEN ph_max >= 7.70
						THEN 4
					END AS ph_score
				,CASE 
					WHEN aado2_max IS NULL
						THEN NULL
					WHEN aado2_max < 200
						THEN 0
					WHEN aado2_max < 350
						THEN 2
					WHEN aado2_max < 500
						THEN 3
					WHEN aado2_max >= 500
						THEN 4
					END AS aado2_score
			FROM cohort
			)
		-- combine together the scores for min/max, using the following rules:
		--  1) select the value furthest from a predefined normal value
		--  2) if both equidistant, choose the one which gives a worse score
		,scorecomp AS (
			SELECT cohort.subject_id
				,cohort.hadm_id
				,cohort.stay_id
				-- the rules for APACHE II require the definition of a "worst" value
				-- this value is defined as whatever value is furthest from a predefined normal value
				-- e.g., for heart rate, worst is defined as furthest from 75
				,CASE 
					WHEN heartrate_max IS NULL
						THEN NULL
					WHEN ABS(heartrate_max - 75) > ABS(heartrate_min - 75)
						THEN smax.hr_score
					WHEN ABS(heartrate_max - 75) < ABS(heartrate_min - 75)
						THEN smin.hr_score
							-- values are equidistant - pick the larger score
					WHEN ABS(heartrate_max - 75) = ABS(heartrate_min - 75)
						AND smax.hr_score >= smin.hr_score
						THEN smax.hr_score
					WHEN ABS(heartrate_max - 75) = ABS(heartrate_min - 75)
						AND smax.hr_score < smin.hr_score
						THEN smin.hr_score
					END AS hr_score
				,CASE 
					WHEN mbp_max IS NULL
						THEN NULL
					WHEN ABS(mbp_max - 90) > ABS(mbp_min - 90)
						THEN smax.mbp_score
					WHEN ABS(mbp_max - 90) < ABS(mbp_min - 90)
						THEN smin.mbp_score
					WHEN ABS(mbp_max - 90) = ABS(mbp_min - 90)
						AND smax.mbp_score >= smin.mbp_score
						THEN smax.mbp_score
					WHEN ABS(mbp_max - 90) = ABS(mbp_min - 90)
						AND smax.mbp_score < smin.mbp_score
						THEN smin.mbp_score
					END AS mbp_score
				,CASE 
					WHEN temp_max IS NULL
						THEN NULL
					WHEN ABS(temp_max - 37) > ABS(temp_min - 37)
						THEN smax.temp_score
					WHEN ABS(temp_max - 37) < ABS(temp_min - 37)
						THEN smin.temp_score
					WHEN ABS(temp_max - 37) = ABS(temp_min - 37)
						AND smax.temp_score >= smin.temp_score
						THEN smax.temp_score
					WHEN ABS(temp_max - 37) = ABS(temp_min - 37)
						AND smax.temp_score < smin.temp_score
						THEN smin.temp_score
					END AS temp_score
				,CASE 
					WHEN resp_max IS NULL
						THEN NULL
					WHEN ABS(resp_max - 19) > ABS(resp_min - 19)
						THEN smax.resp_rate_score
					WHEN ABS(resp_max - 19) < ABS(resp_min - 19)
						THEN smin.resp_rate_score
					WHEN ABS(resp_max - 19) = ABS(resp_max - 19)
						AND smax.resp_rate_score >= smin.resp_rate_score
						THEN smax.resp_rate_score
					WHEN ABS(resp_max - 19) = ABS(resp_max - 19)
						AND smax.resp_rate_score < smin.resp_rate_score
						THEN smin.resp_rate_score
					END AS resp_rate_score
				,CASE 
					WHEN cr_max IS NULL
						THEN NULL
					WHEN ABS(cr_max - 1) > ABS(cr_min - 1)
						THEN smax.cr_score
					WHEN ABS(cr_max - 1) < ABS(cr_min - 1)
						THEN smin.cr_score
					WHEN ABS(cr_max - 1) = ABS(cr_min - 1)
						AND smax.cr_score >= smin.cr_score
						THEN smax.cr_score
					WHEN ABS(cr_max - 1) = ABS(cr_min - 1)
						AND smax.cr_score < smin.cr_score
						THEN smin.cr_score
					END AS cr_score
				,CASE 
					WHEN sodium_max IS NULL
						THEN NULL
					WHEN ABS(sodium_max - 145.5) > ABS(sodium_min - 145.5)
						THEN smax.sodium_score
					WHEN ABS(sodium_max - 145.5) < ABS(sodium_min - 145.5)
						THEN smin.sodium_score
					WHEN ABS(sodium_max - 145.5) = ABS(sodium_max - 145.5)
						AND smax.sodium_score >= smin.sodium_score
						THEN smax.sodium_score
					WHEN ABS(sodium_max - 145.5) = ABS(sodium_max - 145.5)
						AND smax.sodium_score < smin.sodium_score
						THEN smin.sodium_score
					END AS sodium_score
				,CASE 
					WHEN potassium_max IS NULL
						THEN NULL
					WHEN ABS(potassium_max - 4.5) > ABS(potassium_min - 4.5)
						THEN smax.potassium_score
					WHEN ABS(potassium_max - 4.5) < ABS(potassium_min - 4.5)
						THEN smin.potassium_score
					WHEN ABS(potassium_max - 4.5) = ABS(potassium_max - 4.5)
						AND smax.potassium_score >= smin.potassium_score
						THEN smax.potassium_score
					WHEN ABS(potassium_max - 4.5) = ABS(potassium_max - 4.5)
						AND smax.potassium_score < smin.potassium_score
						THEN smin.potassium_score
					END AS potassium_score
				,CASE 
					WHEN hct_max IS NULL
						THEN NULL
					WHEN ABS(hct_max - 45.5) > ABS(hct_min - 45.5)
						THEN smax.hct_score
					WHEN ABS(hct_max - 45.5) < ABS(hct_min - 45.5)
						THEN smin.hct_score
					WHEN ABS(hct_max - 45.5) = ABS(hct_max - 45.5)
						AND smax.hct_score >= smin.hct_score
						THEN smax.hct_score
					WHEN ABS(hct_max - 45.5) = ABS(hct_max - 45.5)
						AND smax.hct_score < smin.hct_score
						THEN smin.hct_score
					END AS hct_score
				,CASE 
					WHEN wbc_max IS NULL
						THEN NULL
					WHEN ABS(wbc_max - 11.5) > ABS(wbc_min - 11.5)
						THEN smax.wbc_score
					WHEN ABS(wbc_max - 11.5) < ABS(wbc_min - 11.5)
						THEN smin.wbc_score
					WHEN ABS(wbc_max - 11.5) = ABS(wbc_max - 11.5)
						AND smax.wbc_score >= smin.wbc_score
						THEN smax.wbc_score
					WHEN ABS(wbc_max - 11.5) = ABS(wbc_max - 11.5)
						AND smax.wbc_score < smin.wbc_score
						THEN smin.wbc_score
					END AS wbc_score
				,CASE 
					WHEN mingcs IS NULL
						THEN NULL
					ELSE 15 - mingcs
					END AS gcs_score
				,CASE 
					WHEN ph_max IS NULL
						THEN NULL
					WHEN ABS(ph_max - 7.40) > ABS(ph_min - 7.40)
						THEN smax.ph_score
					WHEN ABS(ph_max - 7.40) < ABS(ph_min - 7.40)
						THEN smin.ph_score
					WHEN ABS(ph_max - 7.40) = ABS(ph_max - 7.40)
						AND smax.ph_score >= smin.ph_score
						THEN smax.ph_score
					WHEN ABS(ph_max - 7.40) = ABS(ph_max - 7.40)
						AND smax.ph_score < smin.ph_score
						THEN smin.ph_score
					END AS ph_score
				,GREATEST(pao2_score, aado2_score) AS oxygenation_score
				,CASE 
					WHEN age IS NULL
						THEN NULL
					WHEN age < 45
						THEN 0
					WHEN age < 55
						THEN 2
					WHEN age < 65
						THEN 3
					WHEN age < 75
						THEN 5
					WHEN age >= 75
						THEN 6
					END AS age_score
				,CASE 
					WHEN chronic_disease = 0
						THEN 0
					WHEN chronic_disease = 1
						AND admissiontype IN (
							'UnscheduledSurgical'
							,'Medical'
							)
						THEN 5
					WHEN chronic_disease = 1
						AND admissiontype = 'ScheduledSurgical'
						THEN 2
					END AS chronic_health_score
			FROM cohort
			LEFT JOIN score_min smin ON cohort.stay_id = smin.stay_id
			LEFT JOIN score_max smax ON cohort.stay_id = smax.stay_id
			)

-- tabulate the APACHE II using the scores from the worst values
SELECT s.*
	-- coalesce statements impute normal score of zero if data element is missing
	,COALESCE(hr_score, 0) + COALESCE(mbp_score, 0) + COALESCE(temp_score, 0) + COALESCE(resp_rate_score, 0) + COALESCE(oxygenation_score, 0) + COALESCE(hct_score, 0) + COALESCE(wbc_score, 0) + COALESCE(cr_score, 0) + COALESCE(sodium_score, 0) + COALESCE(potassium_score, 0) + COALESCE(ph_score, 0) + COALESCE(age_score, 0) + COALESCE(chronic_health_score, 0) + COALESCE(gcs_score, 0) AS apacheii
FROM scorecomp s