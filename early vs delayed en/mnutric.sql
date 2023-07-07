CREATE MATERIALIZED VIEW PUBLIC.mnutric AS
	-- reference: Rahman A, Hasan RM, Agarwala R, Martin C, Day AG, Heyland DK. 
	-- Identifying critically-ill patients who will benefit most from nutritional therapy: 
	-- Further validation of the “modified NUTRIC” nutritional risk assessment tool. 
	-- Clinical Nutrition. 2016;35(1):158-162. doi:10.1016/j.clnu.2015.01.015
	--
	-- comorbidities were referenced to the Charlson Comorbidity Index (CCI)
	-- https://github.com/MIT-LCP/mimic-code/blob/main/mimic-iv/concepts_postgres/comorbidity/charlson.sql
	WITH diag AS (
			SELECT hadm_id
				,CASE 
					WHEN icd_version = 9
						THEN icd_code
					ELSE NULL
					END AS icd9_code
				,CASE 
					WHEN icd_version = 10
						THEN icd_code
					ELSE NULL
					END AS icd10_code
			FROM mimiciv_hosp.diagnoses_icd
			)
		,com AS (
			SELECT ad.hadm_id
				-- Myocardial infarction
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) IN (
								'410'
								,'412'
								)
							OR SUBSTR(icd10_code, 1, 3) IN (
								'I21'
								,'I22'
								)
							OR SUBSTR(icd10_code, 1, 4) = 'I252'
							THEN 1
						ELSE 0
						END) AS myocardial_infarct
				-- Congestive heart failure
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) = '428'
							OR SUBSTR(icd9_code, 1, 5) IN (
								'39891'
								,'40201'
								,'40211'
								,'40291'
								,'40401'
								,'40403'
								,'40411'
								,'40413'
								,'40491'
								,'40493'
								)
							OR SUBSTR(icd9_code, 1, 4) BETWEEN '4254'
								AND '4259'
							OR SUBSTR(icd10_code, 1, 3) IN (
								'I43'
								,'I50'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'I099'
								,'I110'
								,'I130'
								,'I132'
								,'I255'
								,'I420'
								,'I425'
								,'I426'
								,'I427'
								,'I428'
								,'I429'
								,'P290'
								)
							THEN 1
						ELSE 0
						END) AS congestive_heart_failure
				-- Peripheral vascular disease
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) IN (
								'440'
								,'441'
								)
							OR SUBSTR(icd9_code, 1, 4) IN (
								'0930'
								,'4373'
								,'4471'
								,'5571'
								,'5579'
								,'V434'
								)
							OR SUBSTR(icd9_code, 1, 4) BETWEEN '4431'
								AND '4439'
							OR SUBSTR(icd10_code, 1, 3) IN (
								'I70'
								,'I71'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'I731'
								,'I738'
								,'I739'
								,'I771'
								,'I790'
								,'I792'
								,'K551'
								,'K558'
								,'K559'
								,'Z958'
								,'Z959'
								)
							THEN 1
						ELSE 0
						END) AS peripheral_vascular_disease
				-- Cerebrovascular disease
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) BETWEEN '430'
								AND '438'
							OR SUBSTR(icd9_code, 1, 5) = '36234'
							OR SUBSTR(icd10_code, 1, 3) IN (
								'G45'
								,'G46'
								)
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'I60'
								AND 'I69'
							OR SUBSTR(icd10_code, 1, 4) = 'H340'
							THEN 1
						ELSE 0
						END) AS cerebrovascular_disease
				-- Dementia
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) = '290'
							OR SUBSTR(icd9_code, 1, 4) IN (
								'2941'
								,'3312'
								)
							OR SUBSTR(icd10_code, 1, 3) IN (
								'F00'
								,'F01'
								,'F02'
								,'F03'
								,'G30'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'F051'
								,'G311'
								)
							THEN 1
						ELSE 0
						END) AS dementia
				-- Chronic pulmonary disease
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) BETWEEN '490'
								AND '505'
							OR SUBSTR(icd9_code, 1, 4) IN (
								'4168'
								,'4169'
								,'5064'
								,'5081'
								,'5088'
								)
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'J40'
								AND 'J47'
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'J60'
								AND 'J67'
							OR SUBSTR(icd10_code, 1, 4) IN (
								'I278'
								,'I279'
								,'J684'
								,'J701'
								,'J703'
								)
							THEN 1
						ELSE 0
						END) AS chronic_pulmonary_disease
				-- Rheumatic disease
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) = '725'
							OR SUBSTR(icd9_code, 1, 4) IN (
								'4465'
								,'7100'
								,'7101'
								,'7102'
								,'7103'
								,'7104'
								,'7140'
								,'7141'
								,'7142'
								,'7148'
								)
							OR SUBSTR(icd10_code, 1, 3) IN (
								'M05'
								,'M06'
								,'M32'
								,'M33'
								,'M34'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'M315'
								,'M351'
								,'M353'
								,'M360'
								)
							THEN 1
						ELSE 0
						END) AS rheumatic_disease
				-- liver disease
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) IN (
								'570'
								,'571'
								)
							OR SUBSTR(icd9_code, 1, 4) IN (
								'0706'
								,'0709'
								,'5733'
								,'5734'
								,'5738'
								,'5739'
								,'V427'
								,'4560'
								,'4561'
								,'4562'
								)
							OR SUBSTR(icd9_code, 1, 5) IN (
								'07022'
								,'07023'
								,'07032'
								,'07033'
								,'07044'
								,'07054'
								)
							OR SUBSTR(icd9_code, 1, 4) BETWEEN '5722'
								AND '5728'
							OR SUBSTR(icd10_code, 1, 3) IN (
								'B18'
								,'K73'
								,'K74'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'K700'
								,'K701'
								,'K702'
								,'K703'
								,'K709'
								,'K713'
								,'K714'
								,'K715'
								,'K717'
								,'K760'
								,'K762'
								,'K763'
								,'K764'
								,'K768'
								,'K769'
								,'Z944'
								,'I850'
								,'I859'
								,'I864'
								,'I982'
								,'K704'
								,'K711'
								,'K721'
								,'K729'
								,'K765'
								,'K766'
								,'K767'
								)
							THEN 1
						ELSE 0
						END) AS liver_disease
				-- Diabetes
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 4) IN (
								'2500'
								,'2501'
								,'2502'
								,'2503'
								,'2508'
								,'2509'
								,'2504'
								,'2505'
								,'2506'
								,'2507'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'E100'
								,'E10l'
								,'E106'
								,'E108'
								,'E109'
								,'E110'
								,'E111'
								,'E116'
								,'E118'
								,'E119'
								,'E120'
								,'E121'
								,'E126'
								,'E128'
								,'E129'
								,'E130'
								,'E131'
								,'E136'
								,'E138'
								,'E139'
								,'E140'
								,'E141'
								,'E146'
								,'E148'
								,'E149'
								,'E102'
								,'E103'
								,'E104'
								,'E105'
								,'E107'
								,'E112'
								,'E113'
								,'E114'
								,'E115'
								,'E117'
								,'E122'
								,'E123'
								,'E124'
								,'E125'
								,'E127'
								,'E132'
								,'E133'
								,'E134'
								,'E135'
								,'E137'
								,'E142'
								,'E143'
								,'E144'
								,'E145'
								,'E147'
								)
							THEN 1
						ELSE 0
						END) AS diabetes
				-- Hemiplegia or paraplegia
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) IN (
								'342'
								,'343'
								)
							OR SUBSTR(icd9_code, 1, 4) IN (
								'3341'
								,'3440'
								,'3441'
								,'3442'
								,'3443'
								,'3444'
								,'3445'
								,'3446'
								,'3449'
								)
							OR SUBSTR(icd10_code, 1, 3) IN (
								'G81'
								,'G82'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'G041'
								,'G114'
								,'G801'
								,'G802'
								,'G830'
								,'G831'
								,'G832'
								,'G833'
								,'G834'
								,'G839'
								)
							THEN 1
						ELSE 0
						END) AS paraplegia
				-- Renal disease
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) IN (
								'582'
								,'585'
								,'586'
								,'V56'
								)
							OR SUBSTR(icd9_code, 1, 4) IN (
								'5880'
								,'V420'
								,'V451'
								)
							OR SUBSTR(icd9_code, 1, 4) BETWEEN '5830'
								AND '5837'
							OR SUBSTR(icd9_code, 1, 5) IN (
								'40301'
								,'40311'
								,'40391'
								,'40402'
								,'40403'
								,'40412'
								,'40413'
								,'40492'
								,'40493'
								)
							OR SUBSTR(icd10_code, 1, 3) IN (
								'N18'
								,'N19'
								)
							OR SUBSTR(icd10_code, 1, 4) IN (
								'I120'
								,'I131'
								,'N032'
								,'N033'
								,'N034'
								,'N035'
								,'N036'
								,'N037'
								,'N052'
								,'N053'
								,'N054'
								,'N055'
								,'N056'
								,'N057'
								,'N250'
								,'Z490'
								,'Z491'
								,'Z492'
								,'Z940'
								,'Z992'
								)
							THEN 1
						ELSE 0
						END) AS renal_disease
				-- Any malignancy, including lymphoma and leukemia,
				-- except malignant neoplasm of skin.
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) BETWEEN '140'
								AND '172'
							OR SUBSTR(icd9_code, 1, 4) BETWEEN '1740'
								AND '1958'
							OR SUBSTR(icd9_code, 1, 3) BETWEEN '200'
								AND '208'
							OR SUBSTR(icd9_code, 1, 4) = '2386'
							OR SUBSTR(icd10_code, 1, 3) IN (
								'C43'
								,'C88'
								)
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'C00'
								AND 'C26'
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'C30'
								AND 'C34'
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'C37'
								AND 'C41'
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'C45'
								AND 'C58'
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'C60'
								AND 'C76'
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'C81'
								AND 'C85'
							OR SUBSTR(icd10_code, 1, 3) BETWEEN 'C90'
								AND 'C97'
							THEN 1
						ELSE 0
						END) AS malignant_cancer
				-- AIDS/HIV
				,MAX(CASE 
						WHEN SUBSTR(icd9_code, 1, 3) IN (
								'042'
								,'043'
								,'044'
								)
							OR SUBSTR(icd10_code, 1, 3) IN (
								'B20'
								,'B21'
								,'B22'
								,'B24'
								)
							THEN 1
						ELSE 0
						END) AS aids
			FROM mimiciv_hosp.admissions ad
			LEFT JOIN diag ON ad.hadm_id = diag.hadm_id
			GROUP BY ad.hadm_id
			)
		-- sofa in first 24h of icu stay
		,sofa_max AS (
			SELECT sofa.stay_id
				,MAX(sofa.hr) hr_max
			FROM sofa
			GROUP BY sofa.stay_id
			)
		,sofa_s1 AS (
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
		,sofa AS (
			SELECT s1.*
			FROM sofa_s1 s1
			WHERE s1.sofa_24h IS NOT NULL
			)
		,scorecomp AS (
			SELECT de.subject_id
				,de.hadm_id
				,de.stay_id
				,CASE 
					WHEN age.age IS NULL
						THEN NULL
					WHEN age.age < 50.0
						THEN 0
					WHEN age.age < 75.0
						THEN 1
					WHEN age.age >= 75.0
						THEN 2
					END AS age_score
				,CASE 
					WHEN apacheii.apacheii IS NULL
						THEN NULL
					WHEN apacheii.apacheii < 15
						THEN 0
					WHEN apacheii.apacheii < 20
						THEN 1
					WHEN apacheii.apacheii < 28
						THEN 2
					WHEN apacheii.apacheii >= 28
						THEN 3
					END AS apacheii_score
				,CASE 
					WHEN sofa.sofa_24h IS NULL
						THEN NULL
					WHEN sofa.sofa_24h < 6
						THEN 0
					WHEN sofa.sofa_24h < 10
						THEN 1
					WHEN sofa.sofa_24h >= 10
						THEN 2
					END AS sofa_score
				,GREATEST(
				com.myocardial_infarct, 
				com.congestive_heart_failure, 
				com.peripheral_vascular_disease, 
				com.cerebrovascular_disease, 
				com.dementia, 
				com.chronic_pulmonary_disease, 
				com.rheumatic_disease, 
				com.liver_disease, 
				com.diabetes, 
				com.paraplegia, 
				com.renal_disease, 
				com.malignant_cancer, 
				com.aids) AS comor_score
				,CASE 
					WHEN (datetime_diff(de.icu_intime, de.admittime, 'HOUR') / 24) < 1
						THEN 0
					WHEN (datetime_diff(de.icu_intime, de.admittime, 'HOUR') / 24) >= 1
						THEN 1
					END AS time_score
			FROM icustay_detail de
			LEFT JOIN age ON de.hadm_id = age.hadm_id
			LEFT JOIN apacheii ON de.stay_id = apacheii.stay_id
			LEFT JOIN sofa ON de.stay_id = sofa.stay_id
			LEFT JOIN com ON de.hadm_id = com.hadm_id
			)

SELECT s.*
	,COALESCE(age_score, 0) 
	+ COALESCE(apacheii_score, 0) 
	+ COALESCE(sofa_score, 0) 
	+ COALESCE(comor_score, 0) 
	+ COALESCE(time_score, 0) 
	AS mnutric
FROM scorecomp s