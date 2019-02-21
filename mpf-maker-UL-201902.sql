/* ---------- Unit-Linked Product DCS-Input Generating Script ----------

Author: Bo Lee
Source: STRUC_UL, SQL Actuarial Database
Output: Processed MPF-style data to be sent to DCS

------------------------------------------------------------------------*/
/*      ***** CHANGE LOG *****

January 30, 2019: Added CHANNEL CODE */

/*		***** SUMMARY *****

New UL products have been released that require more sophisticated methods for labeling.
These new products and their associated riders (RU4, RU5 / HI, CM), depending on their
levels of sum assured and annualized premiums paid, carry different commission structures.
(As of Jan 2019, this convention applies only to products sold via Siam Commercial Bank.)
To help differentiate this commission structure in the data, the base policy PLAN codes
are to be given a B/C/D suffix.

Thus, this set of queries is designed to accommodate this procedure set out in Sept 2018.

This script works like this:
(1) Extracts a list of SCB-sold UL base policy numbers that contain both HI and CM riders.
(2) Extracts the annualized premium of base policy and sum assured of the CM rider.
(3) Makes comparisons, and assigns the correct PLAN code for each base polic according to
	conditions fulfilled.
(4) For ease of use, these new PLAN codes replace the old PLAN codes towards the end of
	the script (through join).

		***** INSTRUCTIONS *****

This script, like ones for other product portfolios, is designed to produce the results in
two new tables by just pressing F5 once. So, make sure the four tables listed below are dropped beforehand.

DROP TABLE [DCSMPF_UL_IF2]
DROP TABLE [DCSMPF_UL_NBYTD2]
DROP TABLE [RPUL_PACKCODE_TABLE]
DROP TABLE [UL_BASE_INITSA]

The following reference tables must be updated and existing before running the script:

	Table name		Description
	------------	--------------
	- UL_dist		Distributor code table
	- UL_model		UL policy secondary data table
	- SPcode		SPcode table

		***** MAJOR CHANGE LOG *****

September 2018
	Added queries to accommodate RPUL (RU4, RU5) commission structure system.

January 2019
	Added queries to accommodate 'Protection'/'Savings' product categorization,
	which includes new DCS fields towards the end.
	Some DCS fields, which require aggregate values of both Protection and Savings components,
	have their calculations moved to later queries.
*/

------------------------------- CODE -----------------------------------

-- Some controls
USE ACT_DATA_201902_TEST

-- Condense the STRUC_UL data for only inforce policies and store base policy INIT_SA
SELECT *
INTO #STRUC_UL_IF
FROM struc_ul -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Change DB destination here <<<<<<<<<<<<<<
WHERE [PPSTAT] = 'IF'

-- Since month end of September 2018, it was decided that rider policies will have
-- SA_BASIC that is INITIAL SUM ASSURED of their respective base policies instead of
-- their own INIT_SA values.

-- Store INIT_SA of each base policy
-- Grouped to aggregate PD and SD since they count as 2 parts of 1 base policy

SELECT [POLNO]
	, SUM([INIT_SA])	[INIT_SA]
INTO [UL_BASE_INITSA]
FROM #STRUC_UL_IF
WHERE SUBSTRING([PLAN], 3, 2) in ('LD', 'PD', 'SD')
GROUP BY [polno]

/*  Identify policies that contain both SCB-sold HI and CM riders and create a reference list,
while filtering for HI riders with minimum SA of 10k  */

SELECT * 
INTO #RPUL_POL_LIST
FROM (
	SELECT [POLNO]
	FROM #STRUC_UL_IF WHERE LEFT([PLAN], 2) = 'HI' AND [INIT_SA] >= 10000 AND [partner] = 'SC' and [PACKCODE] IN ('RU5', 'RU6')
	INTERSECT
	SELECT [POLNO]
	FROM #STRUC_UL_IF WHERE LEFT([PLAN], 2) = 'CM' AND [partner] = 'SC' and [PACKCODE] IN ('RU5', 'RU6')
	) as TM

-- Find annualized premium of base policies with POLNO matching the above list.

SELECT B.[POLNO]
	, B.[PACKCODE]
	, B.[PLAN]
	, B.[INIT_SA]
	, B.[OP]
	, B.[MODE]
	, (B.[op]+B.[ehp]+B.[eop]) * (CASE
		WHEN B.[MODE] = 'S' OR B.[MODE] = 'Y' THEN 1
		WHEN B.[MODE] = 'H' THEN 2
		WHEN B.[MODE] = 'Q' THEN 4
		WHEN B.[MODE] = 'M' THEN 12
		ELSE NULL
		END)		[RPUL_BASE_PREM]
INTO #RPUL_BASEPREM
FROM #STRUC_UL_IF B
WHERE SUBSTRING([PLAN], 3, 2) = 'LD'
	AND [MODE] != 'S'		-- FILTERS OUT SINGLE PREMIUM POLICIES (PREREQUISITE OF RPUL)
	AND [POLNO] IN (SELECT * FROM #RPUL_POL_LIST)
	AND [PARTNER] = 'SC'	-- Double checks to make sure base policy is sold by SCB

/* Query for CM rider sum assured, and attach annualized premium values of base policy to 
CM rider policy data. */

SELECT [POLNO]
	, [PACKCODE]
	, [PLAN]
	, [INIT_SA]
INTO #RPUL_POL_DATA
FROM #STRUC_UL_IF
WHERE [POLNO] IN (SELECT * FROM #RPUL_POL_LIST)
	AND LEFT([PLAN], 2) = 'CM'

SELECT B.*
	, P.[RPUL_BASE_PREM]
INTO #RPUL_DATA
FROM #RPUL_POL_DATA B
JOIN #RPUL_BASEPREM P ON B.[POLNO] = P.[POLNO]

/*
CM riders need to check if their initial SA values are above the minimum of 3 x AP of base
policy or 10 mil. If so, then the B/C/D character append depends on the base AP level.

If the PACKCODE does not meet the conditions, then a NULL value is assigned. The resulting
table will contain only policy numbers (unique ID of policies) and their new PACKCODE, if
applicable. This will later be used in the final steps of the main script to transform
PACKCODE of only relevant policies.  */

SELECT [POLNO]
	, (CASE
		WHEN [INIT_SA] < (CASE
			WHEN 3*[RPUL_BASE_PREM] > 10000000	THEN 10000000
			ELSE 3*[RPUL_BASE_PREM]
			END)									THEN NULL
		WHEN [RPUL_BASE_PREM] < 500000				THEN [PACKCODE]+'B'
		WHEN [RPUL_BASE_PREM] >= 2000000			THEN [PACKCODE]+'D'
		ELSE [PACKCODE]+'C'
		END)		[RPUL_PACKCODE]
INTO [RPUL_PACKCODE_TABLE]
FROM #RPUL_DATA



GO 
DROP TABLE #RPUL_POL_LIST
GO 
DROP TABLE #RPUL_BASEPREM
GO
DROP TABLE #RPUL_POL_DATA
GO
DROP TABLE #RPUL_DATA
GO



/*
 ****************************************************************************************
 DROP [UL_PACKCODE_TABLE] -- THIS LOOKUP TABLE CAN BE DELETED LATER
 ****************************************************************************************
-----------------------------------------------------------------------------------------
 Input data:		struc_UL
					Raw data pulled from AS400 used for reporting and analysis purposes.
 Purpose:			Replicate DCS processing with SQL query.
 Output:			Retrieves prophet code along with other data requried for MPF generation.
-----------------------------------------------------------------------------------------
 Process Summary:

 The purpose of this script is to replicate the DCS processing procedure for UL policy data.

*/

-- 'Its not three separate queries anymore.
-- Need to describe the new process for distinguishing protection & savings UL policies.'

/*
 Three separate queries constitute the overall query process. Multiple queries are required
 to eseentially work around the limitations of SQL. (1) Pre-filtering of data improves 
 overall performance and segmenting queries accommodates (2) certain calculations that 
 depend on previously calculated values, joined fields, etc. Initial query results are
 saved into temporary tables to be utilized by subsequent queries -- only the final output
 is stored into an actual table.
 
 The first query first extracts inforce policies and makes key transformations to certain
 data fields, such as premium payment frequency and policyholder sex, as well as breaking up
 date of birth / inforce into day/month/year fields for later use. We also join a table to 
 find the corresponding distributor codes (DIST).

 In the second query, with only inforce policies, we...
 (1) Calculate age at entry by using using DOB/DOI values
 (2) Find corresponding product names (FILENAME) and coverage/payment data by joining UL_MODEL
 (3) Calculate reinsurance values
 (4) Fill values for many fields required for MPF generation

 A third and final query is needed to revise coverage/payment terms gathered during query #2
 (POL_TERM_Y, PRM_PAYBL_Y). We also organize the resulting columns to imitate the format seen
 in current MPF system.

 Special rules and exceptions for processing:

 (1) ALTERNATIVE FILENAMES
 Prophet codes are retrieved from UL_MODEL, but certain policy subtypes activated 
 after 2018/05/15 are given alternative codes (PROD_NAME_REPRICED).

 (2) INCORRECT MODE FOR TOPUP POLICIES
 Some top-up policies have incorrect data on payment mode, since this field seems to have been
 copied directly from their respective base policies. Consequently, top-up components are
 manually given PREM_FREQ of 1.

 (3) SET UP OF LOCAL VARIABLE
 We set up PRM_ALLOC_LEVEL as a local variable so it can be easily changed in the future.

 (4) ROUNDING OF CERTAIN VALUES
 The DCS process rounds several values during processing: (a) modal premiums are rounded down
 to the nearest integer; (b) sum assured values are rounded to the nearest integer; (c) RPR_REASS_PC
 has precision of 6 (which we won't touch for now).

 Local variable @SWITCH_ROUNDING is provided as a "switch" to round raw data values like
 described above to help the analyst have an easier time validating SQL vs DCS data for exactness.
 Set the value to 1 to "turn on" rounding; any other number to "turn off".
*/

------------------------------- CODE -----------------------------------

DECLARE @PRM_ALLOC_LEVEL AS INT
SET @PRM_ALLOC_LEVEL = 500000 -- I'm not sure who set this hardcoded value.

DECLARE @VAL_M AS INT
DECLARE @VAL_Y AS INT
SET @VAL_M = (SELECT MONTH(End_Date_E) FROM Val_Date)
SET @VAL_Y = (SELECT YEAR(End_Date_E) FROM Val_Date)


-- Rounding 
DECLARE @SWITCH_ROUNDING AS INT
SET @SWITCH_ROUNDING = 1

SELECT POLNO		[POLNO]
	, [PACKCODE]	[PACKCODE]
	, [PLAN]		[PLAN]
	, CAST([DOI] AS INT)	[DOI]
    , CAST([DOB1] AS INT)	[DOB1]
    , (CASE
		WHEN @SWITCH_ROUNDING = 1 THEN ROUND([INIT_SA], 0)
		ELSE [INIT_SA]
		END)		[INIT_SA]
    , (CASE
		WHEN @SWITCH_ROUNDING = 1 THEN ROUND([CUR_SA], 0)
		ELSE [CUR_SA]
		END)		[CUR_SA]
    , [term]		[COVERTERM]
    , [term2]		[PAYTERM]
    , [ppstat]		[PPSTAT]
	, CAST([ISS_DTE] AS INT)	[ISS_DTE]
    , CAST([eff_dte] AS INT)	[EFF_DTE]
	, CAST([KEY_DTE] AS INT)	[KEY_DTE]
    , (CASE
		WHEN SEX = 'M' THEN 0
		WHEN SEX = 'F' THEN 1
		ELSE NULL
		END)					[SEX]
    , (CASE
		WHEN MODE = 'S' OR MODE = 'Y' OR RIGHT([PLAN], 2) LIKE 'TD' THEN 1 -- LOOP CORRECTS FOR TOPUP POLICIES
		WHEN MODE = 'H' THEN 2
		WHEN MODE = 'Q' THEN 4
		WHEN MODE = 'M' THEN 12
		ELSE NULL
		END)					[PREM_FREQ]
    , CAST([NEXT_DUE] AS INT)	[NEXT_DUE]
    , (CASE
		WHEN @SWITCH_ROUNDING = 1 THEN FLOOR([OP])
		ELSE [OP]
		END)					[OP]
    , [EHP]
    , [SUB_RISK]
    , [EOP]
    , CAST([EXP_DTE] AS INT)	[EXP_DTE]
    , [URE_COI]					[URE_COI]
    , [URE_TT_UNT]				[UNIT]
    , [ATT_AGE]					[AGE]
    , [URE_TT_UV]				[VAL_UNITS_A]
	, D.[DIST]					[DIST]
    , [BRANCHCODE]				[BRANCHCODE]
    , (CASE
		WHEN RIGHT([PLAN], 2) LIKE 'LD' THEN 'Basic'
		WHEN RIGHT([PLAN], 2) LIKE 'ID' THEN 'Increase'
		WHEN RIGHT([PLAN], 2) LIKE 'TD' THEN 'Topup'
		WHEN RIGHT([PLAN], 2) LIKE 'LA' THEN 'Basic-ADB'
		WHEN RIGHT([PLAN], 2) LIKE 'LT' THEN 'Basic-TPD'
		ELSE NULL
		END)					[COMPONENT]
	, CAST(RIGHT(CAST([DOI] AS INT), 2)	AS INT)			[E_DAY]	-- ONLY FOR ENTRYAGE CALCULATION
	, CAST(LEFT(RIGHT(CAST([DOI] AS INT),4),2) AS INT)	[E_MO]	-- ONLY FOR ENTRYAGE CALCULATION
	, CAST(LEFT(CAST([DOI] AS INT), 4) AS INT)			[E_YR]	-- ONLY FOR ENTRYAGE CALCULATION
	, CAST(RIGHT(CAST([DOB1] AS INT), 2) AS INT)		[B_DAY]	-- ONLY FOR ENTRYAGE CALCULATION
	, CAST(LEFT(RIGHT(CAST([DOB1] AS INT),4),2) AS INT)	[B_MO]	-- ONLY FOR ENTRYAGE CALCULATION
	, CAST(LEFT(CAST([DOB1] AS INT), 4) AS INT)			[B_YR]	-- ONLY FOR ENTRYAGE CALCULATION

  INTO #TEMP_UL1
  FROM #STRUC_UL_IF C
  LEFT JOIN [UL_DIST_MODEL] D ON C.[PARTNER] = D.[PARTNER]
  WHERE PPSTAT = 'IF'


  SELECT  S.SPCODE		[SPCODE]
	-- , (CASE
	-- 	WHEN B_MO < E_MO THEN (CASE
	-- 		WHEN E_MO - B_MO < 6 THEN E_YR - B_YR
	-- 		WHEN E_MO - B_MO = 6 THEN (CASE
	-- 			WHEN B_DAY <= E_DAY THEN E_YR - B_YR + 1
	-- 			ELSE E_YR - B_YR
	-- 			END)
	-- 		ELSE E_YR - B_YR + 1
	-- 		END)
	-- 	WHEN B_MO = E_MO THEN E_YR - B_YR
	-- 	ELSE (CASE
	-- 		WHEN B_MO - E_MO < 6 THEN E_YR - B_YR
	-- 		WHEN B_MO - E_MO = 6 THEN (CASE
	-- 			WHEN B_DAY > E_DAY THEN E_YR - B_YR - 1
	-- 			ELSE E_YR - B_YR
	-- 			END)
	-- 		ELSE E_YR - B_YR - 1
	-- 		END)
	-- 	END)			[AGE_AT_ENTRY] -- THE CORRECT ALGORITHM
	, (CASE
		-- WHEN CONVERT(DATE, CONVERT(NVARCHAR, [B_YR]-543)+'-'+CONVERT(NVARCHAR, [B_MO])+'-'+CONVERT(NVARCHAR, [B_DAY])) < CONVERT(DATE, '1940-04-01') THEN [E_YR] - [B_YR] - 1  
		-- ADDITION OF CODE FOR CALCULATING AAE FOR OLD PEOPLE (THE ABOVE CODE IS ACCURATE. THE BELOW CODE FROM DCS IS INACCURATE.)
		WHEN B_YR < 1940 AND B_MO < 4 THEN E_YR - B_YR - 1  
		WHEN B_MO < E_MO THEN (CASE
			WHEN E_MO - B_MO < 6 THEN E_YR - B_YR
			WHEN E_MO - B_MO = 6 THEN (CASE
				WHEN (B_YR + B_MO*10000 + B_DAY*1000000) <= (E_YR + E_MO*10000 +E_DAY*1000000) THEN E_YR - B_YR + 1 -- ** DCS USES WRONG COMPARISON **
				ELSE E_YR - B_YR
				END)
			ELSE E_YR - B_YR + 1
			END)
		WHEN B_MO = E_MO THEN E_YR - B_YR
		ELSE (CASE
			WHEN B_MO - E_MO < 6 THEN E_YR - B_YR
			WHEN B_MO - E_MO = 6 THEN (CASE
				WHEN (B_YR + B_MO*10000 + B_DAY*1000000) > (E_YR + E_MO*10000 +E_DAY*1000000) THEN E_YR - B_YR - 1 -- ** DCS USES WRONG COMPARISON **
				ELSE E_YR - B_YR
				END)
			ELSE E_YR - B_YR - 1
			END)
		END)			[AGE_AT_ENTRY] -- THE WRONG ALGORITHM CURRENTLY USED BY DCS
	, C.OP * PREM_FREQ		[ANNUAL_PREM_B]
	, C.EHP * PREM_FREQ		[ANNUAL_PREM_XH]
	, C.EOP * PREM_FREQ		[ANNUAL_PREM_XO]
	, C.sub_risk			[ANNUAL_PREM_XH_PC]
	, 0						[ANNUAL_PREM_XO_PC]
	, (C.OP+C.EHP+C.EOP) * PREM_FREQ	[ANNUAL_PREM]
	, E_DAY				[ENTRY_DAY]
	, E_MO				[ENTRY_MONTH]
	, E_YR				[ENTRY_YEAR]
	, 1					[INIT_POLS_IF]
	, C.[COVERTERM]		[POL_TERM_Y]
	, C.[PAYTERM]		[PREM_PAYBL_Y]
	, C.PREM_FREQ		[PREM_FREQ]
	, C.SEX				[SEX]
	, C.CUR_SA			[SUM_ASSURED]
	, C.DIST			[DIST]
	, C.[PLAN]			[PRODUCT]
	, (CASE
		WHEN PC.[RPUL_PACKCODE] IS NULL THEN C.[PACKCODE]
		ELSE PC.[RPUL_PACKCODE]
		END)			[PACKAGE_CODE]  ---- ** NEW CHANGE IN SEPTEMBER 2018 **
	, (CASE
		WHEN CONVERT(DATE, CONVERT(VARCHAR, C.DOI)) < '20180515'
			THEN P.PROD_NAME
		WHEN CONVERT(DATE, CONVERT(VARCHAR, C.DOI)) >= '20180515'
			THEN P.PROD_NAME_REPRICED
		ELSE NULL
		END)		[FILENAME]
	, C.[POLNO]
	, 0				[LOAN_INT_PC]
	, 0				[ADD_TERM_M]
	, 0				[GTD_MIN_DB]
	, 0				[RPR_RIDC]
	, 'N/A'			[RES_CODE]
	, 'N/A'			[CSV_CODE]
	, S.SPCODE		[CHANNEL_CODE]
	, C.INIT_SA
	, C.Unit		[NO_UNIT]	-- WE TOOK OUT DIVIDING BY 100 BECAUSE MPF RESULTS MULTIPLY IT BACK BY 100
	, (CASE
		WHEN (CASE
			WHEN CONVERT(DATE, CONVERT(VARCHAR, C.DOI)) < '20180515'
				THEN P.PROD_NAME
			WHEN CONVERT(DATE, CONVERT(VARCHAR, C.DOI)) >= '20180515'
				THEN P.PROD_NAME_REPRICED
			ELSE NULL
			END) = 'U_SULT' THEN 97
		WHEN (C.OP+C.EHP+C.EOP) * PREM_FREQ < @PRM_ALLOC_LEVEL THEN 95
		ELSE 97
		END)		[ALLOC_PC]
	, ROUND(C.VAL_UNITS_A - 0.00499, 2) [VAL_UNITS]		-- ADDED CODE TO ROUND DOWN TO THE 2ND DECIMAL PLACE (FOLLOWS MOST CLOSELY WITH DCS PROCEDURE)
	, 0				[POL_STATUS]
	, 0				[ACC_CLM]
	, (CASE
	/* If base policy, then use its own INIT_SA. Else (rider policy), then use base INIT_SA from joined table. */
		WHEN RIGHT(C.[PLAN], 2) IN ('PD', 'SD', 'LD') THEN C.INIT_SA
		ELSE I.INIT_SA
		END)		[SA_BASIC]		-- Changed logic flow in Jan 2019 to deal with PD/SD products
	, P.POL_TERM
	, P.POL_TRM_AGE_IND
	, p.PRM_TERM
	, P.PRM_TRM_AGE_IND
	, 'UL'		[PRODUCT_PORT]		-- NOT USED FOR MPF
	, NULL		[MAS_POL]			-- NOT USED FOR MPF
	, NULL		[INDICATOR]			-- NOT USED FOR MPF
	, PC.RPUL_PACKCODE
	, (CASE
		WHEN c.E_YR = @VAL_Y THEN 1
		ELSE 0
		END)					[NB_IND]
  INTO #TEMP_UL2
  FROM #TEMP_UL1 C
  LEFT JOIN [UL_MODEL] P ON C.PACKCODE = P.PACKCODE 
	AND C.[PLAN] = P.PRODUCT 
	AND C.DIST = P.DIST
  LEFT JOIN [SPCODE] S ON C.DIST = S.DIST
  LEFT JOIN [RPUL_PACKCODE_TABLE] PC ON C.POLNO = PC.POLNO  	---- ** NEW CHANGE IN SEPTEMBER 2018 **
  LEFT JOIN [UL_BASE_INITSA] I ON C.POLNO = I.POLNO 			---- ** NEW CHANGE IN SEPTEMBER 2018 **

--------------------------------------------------------------------


SELECT A.SPCODE
	, A.AGE_AT_ENTRY
	, A.[ANNUAL_PREM_B] + ISNULL(C.[ANNUAL_PREM_B], 0)		[ANNUAL_PREM_B]		-- Protection + Savings
	, A.[ANNUAL_PREM_XH] + ISNULL(C.[ANNUAL_PREM_XH], 0)	[ANNUAL_PREM_XH]	-- Protection + Savings
	, A.[ANNUAL_PREM_XO] + ISNULL(C.[ANNUAL_PREM_XO], 0)	[ANNUAL_PREM_XO]	-- Protection + Savings
	, ANNUAL_PREM_XH_PC
	, ANNUAL_PREM_XO_PC
	, A.[ANNUAL_PREM] + ISNULL(C.[ANNUAL_PREM], 0)		[ANNUAL_PREM]	-- Protection + Savings
	, ENTRY_DAY
	, ENTRY_MONTH
	, ENTRY_YEAR
	, INIT_POLS_IF
	, (CASE
		WHEN POL_TRM_AGE_IND = 1 THEN POL_TERM - AGE_AT_ENTRY
		ELSE POL_TERM_Y
		END)		[POL_TERM_Y]
	, (CASE
		WHEN PRM_TRM_AGE_IND = 1 THEN PRM_TERM - AGE_AT_ENTRY
		ELSE PRM_TERM
		END)		[PREM_PAYBL_Y]
	, PREM_FREQ
	, SEX
	, A.[SUM_ASSURED] + ISNULL(C.[SUM_ASSURED], 0)		[SUM_ASSURED]	-- Protection + Savings
	, DIST
	, PRODUCT
	, PACKAGE_CODE
	, [FILENAME]
	, A.POLNO
	, LOAN_INT_PC
	, ADD_TERM_M
	, GTD_MIN_DB
	, RPR_RIDC
	, RES_CODE
	, CSV_CODE
	, ALLOC_PC
	, A.[VAL_UNITS] + ISNULL(C.[VAL_UNITS], 0)		[VAL_UNITS]	-- Protection + Savings
	, A.[INIT_SA] + ISNULL(C.[INIT_SA], 0)		[INIT_SA]	-- Protection + Savings
	, A.[NO_UNIT] + ISNULL(C.[NO_UNIT], 0)		[NO_UNIT]	-- Protection + Savings
	, [POL_STATUS] as [POLICY_STATUS]
	, ACC_CLM
	, A.[SA_BASIC] + ISNULL(C.[SA_BASIC], 0)	[SA_BASIC]	-- Protection + Savings
	, A.[ANNUAL_PREM_B]					AS [ANNUAL_PREM_B_A]
	, ISNULL(C.[ANNUAL_PREM_B], 0)		AS [ANNUAL_PREM_B_C]
	, A.[ANNUAL_PREM_XH]				AS [ANNUAL_PREM_XH_A]
	, ISNULL(C.[ANNUAL_PREM_XH], 0)		AS [ANNUAL_PREM_XH_C]
	, A.[ANNUAL_PREM_XO]				AS [ANNUAL_PREM_XO_A]
	, ISNULL(C.[ANNUAL_PREM_XO], 0)		AS [ANNUAL_PREM_XO_C]
	, A.[ANNUAL_PREM]					AS [ANNUAL_PREM_A]
	, ISNULL(C.[ANNUAL_PREM], 0)		AS [ANNUAL_PREM_C]
	, A.[SUM_ASSURED]					AS [SUM_ASSURED_A]
	, ISNULL(C.[SUM_ASSURED], 0)		AS [SUM_ASSURED_C]
	, A.[VAL_UNITS]						AS [VAL_UNITS_A]
	, ISNULL(C.[VAL_UNITS], 0)			AS [VAL_UNITS_C]
	, A.[INIT_SA]						AS [INIT_SA_A]
	, ISNULL(C.[INIT_SA], 0)			AS [INIT_SA_C]
	-- Calculation only for reinsurance calculations in next query
	, A.[SUM_ASSURED] + ISNULL(C.[SUM_ASSURED], 0)	AS [CUR_SA_TOTAL]
	, A.[NB_IND]
INTO #TEMP_UL3
FROM #TEMP_UL2 A
-- Truncated #TEMP_UL2 but with savings plan data only 
LEFT JOIN (SELECT POLNO
	, ANNUAL_PREM_B
	, ANNUAL_PREM_XH
	, ANNUAL_PREM_XO
	, ANNUAL_PREM
	, SUM_ASSURED
	, VAL_UNITS
	, NO_UNIT
	, INIT_SA
	, SA_BASIC
	FROM #TEMP_UL2 T2
	WHERE [PRODUCT] LIKE 'R7SD') C 
	ON A.POLNO = C.POLNO
		AND A.[PRODUCT] = 'R7PD'
WHERE [PRODUCT] NOT LIKE 'R7SD'

SELECT SPCODE
	, AGE_AT_ENTRY
	, ANNUAL_PREM_B
	, ANNUAL_PREM_XH
	, ANNUAL_PREM_XO
	, ANNUAL_PREM_XH_PC
	, ANNUAL_PREM_XO_PC
	, ANNUAL_PREM
	, ENTRY_DAY
	, ENTRY_MONTH
	, ENTRY_YEAR
	, INIT_POLS_IF
	, POL_TERM_Y
	, PREM_PAYBL_Y
	, PREM_FREQ
	, SEX
	, SUM_ASSURED
	, DIST
	, PRODUCT
	, PACKAGE_CODE
	, (CASE
		WHEN LEFT([PRODUCT], 2) = 'CM' THEN 1
		WHEN [PRODUCT] != 'R1LD' THEN 0
		WHEN [CUR_SA_TOTAL] > 2500000 THEN 1
		ELSE 0
		END)			[RE_IND] -- Protection + Savings (uses combined sum assured)
	, (CASE
		WHEN LEFT([PRODUCT], 2) = 'CM' THEN 100*(1 - (
			(CASE
				WHEN ([CUR_SA_TOTAL]/4) > 500000 THEN 500000
				ELSE [CUR_SA_TOTAL]/4
				END
			+ CASE
				WHEN ([CUR_SA_TOTAL]*0.75) > 1500000 THEN 1500000
				ELSE [CUR_SA_TOTAL]*0.75
				END
			)/([CUR_SA_TOTAL]*2)))
		WHEN [PRODUCT] != 'R1LD' THEN 0
		WHEN CUR_SA_TOTAL > 2500000 THEN ROUND((100*(CUR_SA_TOTAL - 2500000 ) / CUR_SA_TOTAL), 6)
		ELSE 0
		END)		[RPR_REASS_PC] -- Protection + Savings (uses combined sum assured)
	, [FILENAME]
	, POLNO
	, LOAN_INT_PC
	, ADD_TERM_M
	, GTD_MIN_DB
	, RPR_RIDC
	, RES_CODE
	, ALLOC_PC
	, VAL_UNITS
	, CSV_CODE
	, INIT_SA
	, NO_UNIT
	, POLICY_STATUS
	, ACC_CLM
	, SA_BASIC -- Note: SA_Basic should be the base policy's INIT_SA
	, SPCODE			[CHANNEL_CODE]
	, ANNUAL_PREM_B_A
	, ANNUAL_PREM_B_C
	, ANNUAL_PREM_XH_A
	, ANNUAL_PREM_XH_C
	, ANNUAL_PREM_XO_A
	, ANNUAL_PREM_XO_C
	, ANNUAL_PREM_A
	, ANNUAL_PREM_C
	, SUM_ASSURED_A
	, SUM_ASSURED_C
	, VAL_UNITS_A
	, VAL_UNITS_C
	, INIT_SA_A
	, INIT_SA_C
	, NB_IND
INTO [DCSMPF_UL_IF]
FROM #TEMP_UL3

-- Extract NB only policies and store into new table

SELECT *
INTO [DCSMPF_UL_NBYTD]
  FROM [DCSMPF_UL_IF]
WHERE NB_IND = 1
  ORDER BY [FILENAME]

-- Extract R7 policies with R7SD (savings component) but no R7PD (protection component)
-- This edge case shouldn't happen, so this query exists to catch any of these cases.

SELECT * 
INTO [_UL_R7SD_CATCHER]
FROM #struc_ul_if m
where m.[polno] in (select r7.[polno] from (
	SELECT [POLNO]
	FROM #STRUC_UL_IF WHERE [PLAN] = 'R7SD'
	EXCEPT
	SELECT [POLNO]
	FROM #STRUC_UL_IF WHERE [PLAN] = 'R7PD'
	) as R7
	)


-- Drop temp tables
GO
DROP TABLE #TEMP_UL1
GO
DROP TABLE #TEMP_UL2
GO
DROP TABLE #TEMP_UL3
GO 
DROP TABLE #STRUC_UL_IF
--GO
--DROP TABLE [DCSMPF_UL_IF]
--GO
--DROP TABLE [DCSMPF_UL_NBYTD]
--go
--DROP TABLE [_UL_R7SD_CATCHER]
GO
DROP TABLE [RPUL_PACKCODE_TABLE]
GO
DROP TABLE [UL_BASE_INITSA]
