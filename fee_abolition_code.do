* ==============================================================================
* SPATIAL RDD ANALYSIS: SCHOOL FEE ABOLITION STUDY
* Student Exercise - Republic of Akwaaba Education Policy
* ==============================================================================
* This do-file walks you through a complete spatial RDD analysis
* Follow each section carefully and read the interpretation comments
* ==============================================================================

clear all
set more off

* Install required packages (run once)
* ssc install rdrobust
* ssc install rddensity
* ssc install lpdensity

* ==============================================================================
* SECTION 1: DATA LOADING AND SETUP
* ==============================================================================

* Load the dataset (make sure the .dta file is in your working directory)
* Insert the file path where your dataset is saved
cd "/Users/kritzz/Library/CloudStorage/Dropbox/UG folder/Teaching/Summer School CORE-IPD/RDD/STATA Exercise/"
use "spatial_rdd_education_ghana.dta", clear

* Check your working directory
pwd

* Get basic information about the dataset
describe
summarize

* INTERPRETATION: 
* - We have 200 students from the Republic of Akwaaba
* - Treatment (fee_abolition) = 1 if student is in Eastern Districts (x_coord > 50)
* - Running variable (distance_boundary) = distance from policy boundary
* - Key outcomes: enrollment_rate, test_scores, child_marriage

* ==============================================================================
* SECTION 2: DESCRIPTIVE ANALYSIS
* ==============================================================================

* Basic cross-tabulation of treatment
tab fee_abolition

* Summary statistics by treatment status
bysort fee_abolition: sum enrollment_rate test_scores child_marriage

* INTERPRETATION:
* Look at the raw differences in means between treatment and control groups
* These are NOT causal effects yet - they include selection bias!

* Check the distribution of our running variable
histogram distance_boundary, by(fee_abolition) ///
    title("Distribution of Distance from Boundary") ///
    xtitle("Distance from Boundary (negative=west, positive=east)")

* INTERPRETATION:
* The running variable should be continuous around the cutoff
* Any "bunching" might suggest manipulation

* ==============================================================================
* SECTION 3: VISUAL RDD ANALYSIS USING RDPLOT
* ==============================================================================

* Install and load rdrobust package if not already done
* ssc install rdrobust

* RD Plot for School Enrollment
rdplot enrollment_rate distance_boundary, ///
    c(0) p(1) graph_options(title("RD Plot: School Enrollment Rate") ///
    xtitle("Distance from Policy Boundary") ///
    ytitle("Enrollment Rate") ///
    legend(label(1 "Sample average within bin") label(2 "Polynomial fit")))

* INTERPRETATION:
* Look for a clear "jump" at the cutoff (distance = 0)
* The jump represents the treatment effect of fee abolition
* Smooth trends on both sides suggest RDD assumptions hold

* RD Plot for Test Scores  
rdplot test_scores distance_boundary, ///
    c(0) p(1) graph_options(title("RD Plot: Test Scores") ///
    xtitle("Distance from Policy Boundary") ///
    ytitle("Test Scores (0-100)") ///
    legend(label(1 "Sample average within bin") label(2 "Polynomial fit")))

* RD Plot for Child Marriage (females aged 14+ only)
rdplot child_marriage distance_boundary if female == 1 & age >= 14, ///
    c(0) p(1) graph_options(title("RD Plot: Child Marriage Rate") ///
    xtitle("Distance from Policy Boundary") ///
    ytitle("Child Marriage Rate") ///
    legend(label(1 "Sample average within bin") label(2 "Polynomial fit")))

* INTERPRETATION:
* Child marriage should DECREASE with fee abolition (negative treatment effect)
* This analysis only includes females aged 14+ (the relevant population)

* ==============================================================================
* SECTION 5: SPATIAL RDD ESTIMATION - INCORPORATING GEOGRAPHIC CONTROLS
* ==============================================================================

* Traditional RDD uses only the running variable (distance from boundary)
* Spatial RDD can also control for geographic coordinates to improve precision

* Let's start with a simple approach: try different fixed bandwidths

* Enrollment Rate Analysis
* Standard RDD (distance only)

foreach bw in 10 15 20 25 {
    rdrobust enrollment_rate distance_boundary, c(0) p(1) kernel(triangular) h(`bw') vce(hc2)
}

* Spatial RDD with geographic controls
* Create geographic controls that are orthogonal to the running variable
*gen y_coord_centered = (y_coord - 50)/10  // North-South variation (independent of boundary)

* Alternative approach: Use distance from center point instead of raw coordinates
*gen dist_from_center = sqrt((x_coord-50)^2 + (y_coord-50)^2)/10



* Manual spatial RDD with fixed bandwidth
reg enrollment_rate distance_boundary fee_abolition dist_from_center ///
    if abs(distance_boundary) <= 20, robust

* Compare coefficients on fee_abolition between models. what do you notice?


* Now use MSE-optimal bandwidth selection (standard approach)
rdrobust enrollment_rate distance_boundary, c(0) p(1) kernel(triangular) bwselect(mserd) vce(hc2)


* For spatial controls, use distance from center (avoids multicollinearity)

rdrobust enrollment_rate distance_boundary, c(0) p(1) kernel(triangular) ///
    bwselect(mserd) vce(hc2) covs(dist_from_center)

* - Distance from center controls for general proximity to urban core
* - This avoids multicollinearity while capturing spatial heterogeneity
* - Isolates boundary effects from center-periphery gradients

* ==============================================================================
* SECTION 6: SPATIAL RDD ESTIMATION FOR ALL OUTCOMES
* ==============================================================================

* Test Scores Analysis - Compare Standard vs Spatial RDD

* Standard RDD
rdrobust test_scores distance_boundary, c(0) p(1) kernel(triangular) bwselect(mserd) vce(hc2)


* Spatial RDD with geographic controls
rdrobust test_scores distance_boundary, c(0) p(1) kernel(triangular) ///
    bwselect(mserd) vce(hc2) covs(dist_from_center)

* Child Marriage Analysis (females 14+ only)

* Standard RDD
    rdrobust child_marriage distance_boundary if female == 1 & age >= 14, ///
        c(0) p(1) kernel(triangular) bwselect(mserd) vce(hc2)

* Spatial RDD
     rdrobust child_marriage distance_boundary if female == 1 & age >= 14, ///
        c(0) p(1) kernel(triangular) bwselect(mserd) vce(hc2) covs(dist_from_center)
    

	
*What is the issue here?
	
* Check how many observations we have for this analysis
count if female == 1 & age >= 14 & child_marriage != .


    
    * Show descriptive statistics instead
    bysort fee_abolition: sum child_marriage if female == 1 & age >= 14


* INTERPRETATION:
* - For enrollment and test scores: expect POSITIVE treatment effects
* - For child marriage: expect NEGATIVE treatment effect (reduction)
* - RDD is data intensive an observations on margins may not be computable otherwise.
* - Look at "Robust" coefficient and confidence intervals
* - P-values test null hypothesis of no treatment effect

* ==============================================================================
* SECTION 7: ROBUSTNESS - CLUSTERED STANDARD ERRORS AND SPATIAL CORRELATION
* ==============================================================================

* In spatial data, observations close to each other may be correlated
* We account for this by using community clustering 


* Standard clustered standard errors at community level

rdrobust enrollment_rate distance_boundary, c(0) p(1) kernel(triangular) ///
    bwselect(mserd) vce(cluster community_id)


* Spatial RDD with community clustering

rdrobust enrollment_rate distance_boundary, c(0) p(1) kernel(triangular) ///
    bwselect(mserd) vce(cluster community_id) covs(dist_from_center)
	


* "INTERPRETATION:"
* "- Compare standard errors across specifications"
* "- Spatial controls (dist_from_center) can improve precision"
* "- Clustering accounts for correlation within communities"


* ==============================================================================
* SECTION 8: BALANCE TESTS WITH SPATIAL CONTROLS
* ==============================================================================

* Test whether pre-treatment characteristics are smooth at the boundary
* Compare balance tests with and without spatial controls


* Test 1: Household Wealth Index
* Balance Test 1: Household Wealth Index
* Standard RDD:
rdrobust wealth_index distance_boundary, c(0) p(1) kernel(triangular) bwselect(mserd) vce(hc2)
* Spatial RDD:
rdrobust wealth_index distance_boundary, c(0) p(1) kernel(triangular) ///
    bwselect(mserd) vce(hc2) covs(dist_from_center)

* "INTERPRETATION: Both should show p > 0.05 (no discontinuity)"


* Test 2: Parental Education
* Balance Test 2: Parental Education 
* Standard RDD:
rdrobust parent_education distance_boundary, c(0) p(1) kernel(triangular) bwselect(mserd) vce(hc2)
* Spatial RDD:
 rdrobust parent_education distance_boundary, c(0) p(1) kernel(triangular) ///
    bwselect(mserd) vce(hc2) covs(dist_from_center)

* "INTERPRETATION: Both should show p > 0.05 (no discontinuity)"


* Test 3: Student Age
* Balance Test 3: Student Age
* "Standard RDD:"
rdrobust age distance_boundary, c(0) p(1) kernel(triangular) bwselect(mserd) vce(hc2)
* "Spatial RDD:"
rdrobust age distance_boundary, c(0) p(1) kernel(triangular) ///
    bwselect(mserd) vce(hc2) covs(dist_from_center)

* "INTERPRETATION: Both should show p > 0.05 (no discontinuity)"

* Now try running the balance tests for other relevant variables? What do you find?

* OVERALL INTERPRETATION OF BALANCE TESTS:
* - Standard and spatial RDD should give similar conclusions
* - If results differ significantly, it suggests geography matters
* - Spatial controls can help isolate the boundary effect

* ==============================================================================
* SECTION 8: McCRARY DENSITY TEST (Test for Manipulation)
* ==============================================================================

* Test whether there's manipulation of the running variable
* Students/families shouldn't be able to precisely control their side of boundary



* Install rddensity and its dependency if not already installed
* ssc install rddensity
* ssc install lpdensity

* Run the density test
rddensity distance_boundary, c(0) plot

* INTERPRETATION:
* - Tests null hypothesis: no manipulation (smooth density)
* - P-value > 0.05 suggests no manipulation
* - Visual inspection: look for smooth density around cutoff
* - "Bunching" on one side would suggest manipulation
* - In our case, geographic boundaries should be hard to manipulate


* ==============================================================================
* SECTION 9: ADDITIONAL EXERCISES FOR STUDENTS
* ==============================================================================

* 1. Try different polynomial orders (p=2, p=3) - does it change results?
* 2. Try different kernels (uniform, epanechnikov) - are results robust?
* 3. Examine heterogeneous effects by gender or age groups
* 4. Test for effects at placebo cutoffs (e.g., distance = -10 or distance = 10)
* 5. Create your own RD plots with different bin sizes using rdplot options




* ==============================================================================
* END OF ANALYSIS
* ==============================================================================

* ANALYSIS COMPLETE!
* Review your results and consider the policy implications:
* - Did fee abolition increase enrollment and test scores?
* - Did it reduce child marriage rates?
* - Are the effects economically significant?
* - What are the limitations of this analysis?
