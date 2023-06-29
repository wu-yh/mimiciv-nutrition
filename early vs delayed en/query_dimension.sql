-- query itemid
SELECT *
FROM mimiciv_icu.d_items
WHERE label ~ * 'enteral'
	OR label ~ * 'parenteral'
	OR label ~ * 'nutrition'

SELECT *
FROM mimiciv_icu.d_items
WHERE label ~ * 'calor'

SELECT *
FROM mimiciv_icu.d_items
WHERE label ~ * 'insulin'

SELECT *
FROM mimiciv_icu.d_items
WHERE label ~ * 'residual'

SELECT *
FROM mimiciv_hosp.d_labitems
WHERE label ~ * 'c-reactive protein'
	OR label ~ * 'crp'
	
SELECT *
FROM mimiciv_hosp.d_labitems
WHERE label ~* 'cholesterol'

SELECT *
FROM mimiciv_icu.d_items
WHERE label ~* 'protein'


SELECT * 
FROM mimiciv_hosp.d_labitems 
WHERE label ~* 'triglyceride'

SELECT * 
FROM mimiciv_hosp.d_labitems 
WHERE label ~* 'phosphate'

	-- EN: 226221	Enteral Nutrition Ingredient	Enteral Nutrition Ingredient	ingredientevents	Ingredients	mL	Ingredient
	-- PN: 227079	Parenteral Nutrition Ingredient	Parenteral Nutrition Ingredient	ingredientevents	Ingredients	mL	Ingredient
	-- calorie: 
	-- 220412	Kilogram calory	Kcal	ingredientevents	Ingredients - general (Not In Use)	Kcal	Ingredient
	-- 226060	Calories	Calories	ingredientevents	Ingredients	Kcal	Ingredient
	-- protein: 220454	Protein	Protein	ingredientevents	Ingredients	grams	Ingredient
	-- insulin:
	-- 223257	Insulin - 70/30	Insulin - 70/30	inputevents	Medications	units	Solution
	-- 223258	Insulin - Regular	Insulin - Regular	inputevents	Medications	units	Solution
	-- 223259	Insulin - NPH	Insulin - NPH	inputevents	Medications	units	Solution
	-- 223260	Insulin - Glargine	Insulin - Glargine	inputevents	Medications	units	Solution
	-- 223261	Insulin - Humalog 75/25	Insulin - Humalog 75/25	inputevents	Medications	units	Solution
	-- 223262	Insulin - Humalog	Insulin - Humalog	inputevents	Medications	units	Solution
	-- 229299	Insulin - Novolog	Insulin - Novolog	inputevents	Medications	units	Solution
	-- 229619	Insulin - U500	Insulin - U500	inputevents	Medications	units	Solution
	-- GRV:
	-- 227510	TF Residual	TF Residual	outputevents	Output	mL	Numeric
	-- 227511	TF Residual Output	TF Residual Output	outputevents	Output	mL	Numeric
	-- CRP:
	-- 50889	C-Reactive Protein	Blood	Chemistry
	-- 51652	High-Sensitivity CRP	Blood	Chemistry
	-- total cholesterol: 50907	Cholesterol, Total	Blood	Chemistry
	--
	-- triglyceride: 51000	Triglycerides	Blood	Chemistry
	-- phosphate: 50970	Phosphate	Blood	Chemistry


