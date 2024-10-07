/* Data Processing (Cleaning and Transformation) */
/* 1. Split a column into two */
# Add the new columns inside my table
ALTER TABLE demographics_info 
  ADD lastName VARCHAR(255),
  ADD firstName VARCHAR(255);
  
# Update the values into the columns
UPDATE demographics_info 
SET 
 lastName = SUBSTRING_INDEX(name, ';', 1),
 firstName = SUBSTRING_INDEX(name, ';', -1);

# Delete the name Column from table 
ALTER TABLE demographics_info 
DROP COLUMN name;
/* Verify Results */
SELECT * FROM demographics_info;

/* Make the FinalWorth Colum in full Billions rather than thousands*/
UPDATE demographics_info
SET finalWorth = finalWorth * 1000 * 1000;

/* Checking and Correcting Errors in Name*/
SELECT * 
FROM demographics_info
WHERE firstName LIKE "Fran%";

UPDATE demographics_info
SET firstName = "Francois"
WHERE firstname = "FranÃƒÂ§ois";

/* Extracting New Variables from brithDate Column */
ALTER TABLE demographics_info
  ADD  birthYear INT,
  ADD  birthMonth VARCHAR (255),
  ADD  birthDay INT,
  ADD  age INT;
  
/* Add values into columns */
UPDATE demographics_info
SET 
   birthYear = YEAR(birthDate),
   birthDay = DAY(birthDate),
   age = 2024 - birthYear;

UPDATE demographics_info
SET 
   birthMonth = CASE 
       WHEN MONTH(birthDate) = 1 THEN 'January'
       WHEN MONTH(birthDate) = 2 THEN 'February'
       WHEN MONTH(birthDate) = 3 THEN 'March'
       WHEN MONTH(birthDate) = 4 THEN 'April'
       WHEN MONTH(birthDate) = 5 THEN 'May'
       WHEN MONTH(birthDate) = 6 THEN 'June'
       WHEN MONTH(birthDate) = 7 THEN 'July'
       WHEN MONTH(birthDate) = 8 THEN 'August'
       WHEN MONTH(birthDate) = 9 THEN 'September'
       WHEN MONTH(birthDate) = 10 THEN 'October'
       WHEN MONTH(birthDate) = 11 THEN 'November'
       WHEN MONTH(birthDate) = 12 THEN 'December'
       ELSE 'Unknown'
   END;

/* Delete category variable from the table as it is same as industries*/
ALTER TABLE demographics_info
 DROP COLUMN category;

/* check update of everything so far */
SELECT *
FROM demographics_info;

/* Detect Outilers in age and birthyear variables */
/* Calculate the Zscore as the threehold to detect outilers*/
SELECT age,
birthYear,
lastName,
(age-AVG(age) OVER())/ STDDEV(age) OVER() as Zscore
FROM demographics_info;

/* Using the Zscore to check for values in the age and birthyear that falls outside (above/below) 2SD from the mean */
SELECT * FROM
(SELECT 
age,
birthYear,
lastName,
(age-AVG(age) OVER())/ STDDEV(age) OVER() as Zscore
FROM demographics_info) score_table
WHERE Zscore > 1.96 OR Zscore < -1.96;

/* Remove Outilers from the Age and birthYaear Columns */
UPDATE demographics_info
SET 
  age = 95,
  birthYear = 1929
WHERE age = -5 AND birthYear = 2029;

UPDATE demographics_info
SET 
  age = 96,
  birthYear = 1928
WHERE age = -4 AND birthYear = 2028;

/* Verify Results */
SELECT *
FROM demographics_info
WHERE age = -5 OR age = -4;


/* Performing Joints to combine both demographic data and economic indicators data*/
SELECT *
FROM billionaire.demographics_info
JOIN  billionaire.economic_indicators
 ON demographics_info.country = economic_indicators.country;
 
 /* Remove Duplicates due to Join operation */
CREATE TABLE billionaire_data AS
SELECT DISTINCT
    di.country,
    di.finalWorth,
    di.city,
    di.source,
    di.industries,
    di.selfMade,
    di.gender,
    di.birthDate,
    di.lastName,
    di.firstName,
    di.birthYear,
    di.birthMonth,
    di.birthDay,
    di.age,
    ei.cpi_country,
    ei.gdp_country,
    ei.life_expectancy_country,
    ei.tax_revenue_country_country,
    ei.total_tax_rate_country,
    ei.population_country
FROM billionaire.demographics_info di
JOIN billionaire.economic_indicators ei
ON di.country = ei.country;


/* Billionaire-Level Insights and Analysis  - 1. Demographics*/
/*What is the average age of billionaires across industries?*/
SELECT industries, ROUND(AVG(age), 0) as Average_Age
FROM billionaire_data
GROUP BY industries
ORDER BY Average_Age;

/* Distrbtion of Billionaries based on Gender*/
SELECT gender, COUNT(*) as Billionaire_Count
FROM billionaire_data
GROUP BY gender;

/* Top 10 Countries with Most Billionaires*/
SELECT country, COUNT(*) as Billionaire_Count
FROM billionaire_data
GROUP BY country
ORDER BY Billionaire_Count DESC
LIMIT 10;

/* BirthMonth Distrubtion of Billionaires */
SELECT birthMonth, COUNT(*) as Billionaire_Count
FROM billionaire_data
GROUP BY birthMonth
ORDER BY Billionaire_Count DESC;

/* Are Billionaires More of selfmade or their wealth is inherent ?*/
SELECT 
  selfMade, 
  COUNT(*) AS Billionaire_Count,
  ROUND((COUNT(*) * 100.0) / (SELECT COUNT(*) FROM billionaire_data), 0) AS Percentage
FROM billionaire_data
GROUP BY selfMade;

/* How does self-made status correlate with age or industry, and does it impact wealth accumulation?*/
/* For this we do a direct insight analysis as MySQL does not allow directly for doing a Correlation Analysis*/
# Age
SELECT selfMade, ROUND(AVG(age), 0) As Average_Age
FROM billionaire_data
GROUP BY selfMade;
# Wealth
SELECT selfMade, ROUND(SUM(finalWorth), 0) As Average_Worth
FROM billionaire_data
GROUP BY selfMade;
/* Indstry - eg checking Fashion & Retail */
SELECT selfMade, industries, COUNT(*) As Billionaire_Count 
FROM billionaire_data
WHERE industries = 'Fashion & Retail'
GROUP BY selfMade, industries;

/* 2. Wealth and Industry: */
/* Which industries dominate among billionaires ?*/
SELECT industries, COUNT(*) as Billionaire_Count
FROM billionaire_data
GROUP BY industries
ORDER BY Billionaire_Count DESC;

/* How does the total wealth of billionaires differ across industries and regions?*/
SELECT industries, SUM(finalWorth) as Billionaire_Wealth
FROM billionaire_data
GROUP BY industries
ORDER BY Billionaire_Wealth DESC;
# By Region 
SELECT country, SUM(finalWorth) as Billionaire_Wealth
FROM billionaire_data
GROUP BY country
ORDER BY Billionaire_Wealth desc
LIMIT 10;

/* Are they any emerging industries creating new billionaires and impact on wealth distrubtion
Spot out emergeing industries through the SelfMade and Age Columns 
By defining a parameter that meets young age and selfmade equal to true */

ALTER TABLE billionaire_data
ADD young_selfmade VARCHAR(255);

UPDATE billionaire_data
SET 
  young_selfmade = CASE
         WHEN selfMade = 'True' AND age <= 50 THEN 'Yes'
         ELSE 'No' END;

SELECT industries, COUNT(*) as Billionare_Count
FROM billionaire_data
WHERE young_selfmade = 'Yes'
GROUP BY industries
ORDER BY Billionare_Count DESC;
# Wealth Distrubtion for emerging Industries 
SELECT industries, SUM(finalWorth) as Billionare_Wealth
FROM billionaire_data
WHERE young_selfmade = 'Yes'
GROUP BY industries
ORDER BY Billionare_Wealth DESC;

/* 3. Wealth and Gender*/
/* How does gender representation vary among billionaires across different industries and countries?*/
SELECT gender, country, COUNT(*) as Billionaire_Count
FROM billionaire_data
GROUP BY gender, country;

SELECT gender, industries, COUNT(*) as Billionaire_Count
FROM billionaire_data
GROUP BY gender, industries;

/*What is the average wealth by gender across industries? Are male or female billionaires wealthier on average?*/
SELECT gender, industries, ROUND(AVG(finalWorth), 0) as Average_Wealth
FROM billionaire_data
GROUP BY gender, industries;



/* Country-wise Economic Indicators Insights and Analysis*/ 
/* 1. Geography */
/*How do economic factors differ between countries with more billionaires and those with fewer?*/
# For top 10 countries with most Billionaires
SELECT 
      AVG(life_expectancy_country),
      AVG(cpi_country),
      AVG(gdp_country),
      AVG(tax_revenue_country_country),
      AVG(total_tax_rate_country),
      AVG(population_country)
FROM billionaire_data
WHERE country IN('United States', 'China', 'United Kingdom', 
                  'Germany', 'India', 'Switzerland', 'Russia',
                  'France', 'Australia', 'Italy');

# For top 10 countries with least Billionaires
SELECT 
      AVG(life_expectancy_country),
      AVG(cpi_country),
      AVG(gdp_country),
      AVG(tax_revenue_country_country),
      AVG(total_tax_rate_country),
      AVG(population_country)
FROM billionaire_data
WHERE country IN('Chile', 'Colombia', 'Egypt', 
                  'Malaysia', 'Netherlands', 'New Zealand', 'Norway',
                  'Poland', 'Turkey', 'Ukraine');

/* 2. Economic Impact*/
/*Is there a pattern between countries with higher tax rates and the number of billionaires?*/
/* Classify Tax Rate into High or Low based on the threshold in this case using the mean */
ALTER TABLE billionaire_data
ADD  taxrate_classification VARCHAR(255);

UPDATE billionaire_data
SET 
	taxrate_classification = CASE WHEN total_tax_rate_country >= 43 THEN 'High' ELSE 'Low' END;

SELECT taxrate_classification, COUNT(*) Billionaire_Count
FROM billionaire_data
GROUP BY taxrate_classification;

/* Do billionaires from high-tax countries have significantly different wealth than those from low-tax countries?"*/
SELECT taxrate_classification, SUM(finalWorth) Billionaire_Worth
FROM billionaire_data
GROUP BY taxrate_classification;









