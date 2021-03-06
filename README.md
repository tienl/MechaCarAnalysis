# MechaCar Analysis

In this repository I demonstrate techniques using R and statistical analysis to provide analysis of a car manufacturer and its dataset.  The objectives were answering some key questions via MPG regression analysis, provide a statistics summary table for suspension coil data.  Use T-Test to analyze our sampling data against population data.  Then finally design a study to further show case what's possible and the lessons from the module.  

## MPG Regression questions and answers
Q: Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
A: The 4 variables that provided non-random amount of variance to the mpg values in the dataset are vehicle length, vehicle weight, spoiler angle, and ground clearance.  The data we have from these 4 variables are continuous.  I've excluded AWD data as it is either true or false and categorical, therefore it is not suited for linear regression analysis.

Q: Is the slope of the linear model considered to be zero? Why or why not?
A: The slop of the linear model is not zero because the p-value is 2.277x10-11, which is much smaller than a typical assumed significance level of 0.05%.  Therefore, we can state that there is sufficient evidence to reject our null hypothesis, which means that the slope of our linear model is not zero.

Q: Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not?
A: The model is fairly accurate with a R value or confidence level of 70% accuracy.  This means in the future prediction, there's about a 70% chance that our prediction will be correct.  We can say that factors in length, and ground clearance for MechaCar prototype models have a greater impact to MPG.  MechaCar Vehicles' spoiler angle and weight in our prototype data reveals these 2 factors do not correlate to MPG.  

## Suspension Coil Summary:
By producing a summary table of mean, median, variance, and standard deviation, we can easily see and compare the manufacturing lots.  While the mean and median of the 3 lots were very similar to within 1 or 2 PSI, the variance and standard deviation tells a different story.  Manufacturing lot 1 has a much more uniform production result compared to lot 3, which has the highest range of variance and standard deviation.  MechaCar's spec dictates that the variance can not exceed 100 PSI, so lot 3 having a 220 PSI variance fails the test.  While Lot 1 and lot 2 are within acceptable ranges, 1.15 and 10.1 PSI variance respectively.  

## Suspension Coil T-Test:
By generating a sampling data, and comparing it to the population using t-test, we can see if our sample data is statistically similar or not.  After several tries, we can see that our sample data has a high p value in general much higher than .05 commonly accepted.  Therefore, our null hypothesis is accepted as our sample data mean PSI is similar to the population mean PSI of 1500.  

## Design Your Own Study:
As a consumer, one would be interested to know the repair cost before purchase.  If we can devise a study to show the repair cost of MechaCar compared to competitors as lower, then it should help with sales by boosting consumer interest and confidence.  

##### Question to ask:
For 100k miles driven, what is the average cost to repair for a manufacturer's car?  Is MechaCar cheaper to repair up to 100K miles compared to competitors?  

**H0:** Means of all groups repair cost are equal.<br />
**Ha:** At least one of the means (hopefully MechaCar's) is different from all other groups.  

**Data needed:** Cars that are 100k miles, data by manufacturer, and their repair cost.

**The testing method would be a two-way ANOVA test.**
  * The dependent variable would be repair cost
  * The indendent variable would be manufacturer  

We can use the R function aov() and summary() to determine the result of our hypothesis.  Then further, use TukeyHSD() to determine our result that shows  comparison, as shown by using my sample data (repair.csv) and code.  The resulting diff column would tell us how much cheaper is each manufacturer compared against each other.

**Example using mock data:**
```
TukeyHSD(aov(repair ~ manufacturer, repairdf))
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = repair ~ manufacturer, data = repairdf)

$manufacturer
       diff        lwr       upr     p adj
b-a  3643.0  -640.2683 7926.2683 0.1104948
c-a  3359.0  -924.2683 7642.2683 0.1536978
d-a  -535.6 -4818.8683 3747.6683 0.9837408   < win
c-b  -284.0 -4567.2683 3999.2683 0.9974759
d-b -4178.6 -8461.8683  104.6683 0.0571360   < win
d-c -3894.6 -8177.8683  388.6683 0.0814884   < Win
```
As shown, manufacturer D is cheaper to repair compared to other all other manufacturers (a, b, c).  With our finding confidence p-value all above .05.  
