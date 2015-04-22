# html texts
welcome_page=
 "<div style='border:1px solid lightgrey; padding:15px;'>

<h3>The Explorer</h3>
<p>This application allows the user to analyse the influence of natural factors on the global surface
temperature. 
The Tutorial gives a step by step introduction to the way the applet works. In the last step of the
tutorial you will find a simple but complete version of the applet to help you investigate
the development of hte global temperature.</p>
<p><em>Best viewed full screen.</em></p>
<p>The full applet contains extra features that are of interest to the advanced user. 
Not all of those features are fully explained.</p>
<p>In version 0.3 a prediction tab is/was added. This is experimental, and it might not always be there. </p>
<p><em>NOTE: this application runs 'in the cloud', and it seems sleepy at times. 
Switching from tabs a couple of times usually wakes it up.</em><p>

<p>If you want to leave comments, go to <a href='http://mrooijer.wordpress.com'>MrOoijer dot wordpress.com</a>.</p>

</div>

<h4>Colophon</h4>
<p>This application was conceived and written in november 2014. A similar application
(see [4] in the details tab) inspired me to make this interactive version of an R library I had written.</p>
<p>The current version is 0.5.1.a - dated 2015-04-18, 11:00 hrs. </p>
<h4>Data is current</h4><p>The data is usually uploaded fresh and brought up-to-date for the latest version of the applet.
See the details tab for the sources used.</p>
<h4>Plans for future versions</h4><ul>
<li>More data</li>
<li>Forecasts</li>
<li>Inspection of values</li>
<li></li>
</ul>
<hr>
<p>Copyright by Jan van Rongen, 2014<p>

<h5>Fixes and extensions</h5><small>
<dl>
<dt>0.5.1 2015-02-22</dt><dd>Button to download graphs. Clean-up some minor editorial things. Choice of algorithms for filtering.</dd>
<dt>0.4.1 2015-01-07</dt><dd>More data series. Small changes to applet interface. More colours. Forecast disabled until it is improved.</dd>
<dt>0.3.0 2014-12-07</dt><dd>Changes to tutorial and applet after feedback.</dd>
<dt>0.2.2 2014-12-05</dt><dd>Confidence intervals as option.</dd>
<dt>0.1.1 2014-11-23</dt><dd>First public version. Added PDO, expanded tutorial</dd>
<dt></dt><dd></dd></dl></small>"

tutorial1.1=
 "<h4>Sources</h4><p>A number of scientific institutes keep track of the global temperature. Each month
they get the data from hundreds of weather stations around
the world. That data is processed to form a map of all the
whole globe with the so called anomaly temperature for that month.
The anomaly is the difference from the average month temperature at that location </p>
<hr>
"
tutorial1.2=
 "<hr><p>For the above diagram, you can choose between datasets and change the display: both the period and the
range of the y-axis can be changed. So you can zoom in on a detail of a chart. 
The original data points will always remain visible, but you can add a line with averaged data. The smoothing slider
averages over consecutive months. The smoothing parameter is the number of months it averages.
</p>
<h4>Aim</h4><p>Our aim is to find as many factors as possible that explain the seemingly
chaotic increase in warming. In the remainder of the tutorial we will go through the possible
factors one by one, beginning with increased CO2. </p>
<h4>Details</h4>
 <p>Different sources use different baseline periods for the calculations of the anomalies, 
 here we have recalculated those to the base period of 1986-2005.</p>
 <p>Institutes use different groups of weather stations for the land temperatures, 
 but for the temperatures on the sea - which are obviously a lot harder to collect-
 there is more overlap between the different sources. Finally there are historical reconstructions
 , some of them going back to the 18th century. Data from before 1880 is considered less reliable
 , that is one reason for not showing it.</p><p>In the remainder of this tutorial we will use the GISS data. Overview of the data used
 and the abbreviations:</p>
 <dl>
 <dt> GISS </dt>
 <dd> The Goddard Institute for Space Studies (NASA, US)</dd>
 <dt> NOAA </dt>
 <dd> National Oceanic and Atmospheric Administration(Federal Agency, US)</dd>
 <dt> HADcrut </dt>
 <dd> The Hadley Climate Research Unit Temperature series (UK)</dd>
 <dt> JMA </dt>
 <dd> The Japanese Meteorological Association (Japan)</dd>
 <dt> C&W </dt>
 <dd> Cowtan & Way, who published corrections to the Hadcrut series and who still 
 update these corrections every month. </dd>
 </dl>"

tutorial2.1=
 "<h4>Rising temperatures and CO2 levels</h4><p>Since 1880 the earth temperature has been rising slowly. 
 By how much? Clearly the temperature has a lot of variation, and it also seems to go up and down a few times. 
 Actually, we will find the cause for that later. But first we want to
 estimate how much the temperature has risen. </p><hr>"

tutorial2.2=
 "<hr><p>
 In the diagram above we drew a straight line through the points as good as we could. Gauss and Legrendre
 in the 19th century discovered the way to do that. The method is called regression or (ordinary) least squares. It finds a line that increases about 0.9 degree Celcius. 
 </p><h4>CO2</h4><p>Modern physics tells us that one of the major causes of warming is the increase in CO2 in the atmosphere. 
 In fact, the increase should be proportional to the log of the level of CO2. 
 In this diagram you can see how much CO2 contributes to the warming: 
 with the radio buttons see the (contribution of the) increasing CO2 level instead of the straight line. 
 </p>
<h4>Changes to the regression</h4><p>With the first slider change the period for
the regression. Choosing a shorter period will decrease our confidence in the result. 
Watch the 'fit' statistic. The other option is to 'smooth' the input data. This always increases the fit somewhat, but the
level of change should stay the same.</p>
<p>The grey band is our total confidence (always with respect to the original points). 
When you increase the smoothing, the fit increases but the confidence decreases. 
This sounds paradoxal, but is not if you consider it a bit further. Otherwise see the details tab. </p>
<h4>TCR</h4>
 <p>The TCR is a measure for the response of the climate to increasing CO2 (and other greenhouse gasses). It is the expected increase in 
 temperature for when the CO2 had doubled from its' 1880 value. Most of our plots show a TCR of about 2.2 degree Celcius.</p>
 <small><p>Note: as we use the log of the CO2 level, which makes the TCR very simple to calculate:
 it is log(2)* the regression coefficient of that level.</p></small>"

tutorial2.3=
  " <p>Note: this part is not clear yet. feedback?.... </p>
<p>In the previous part I briefly mentioned confidence and fit. For time series these are somewhat conflicting indicators. 
The 'fit' just looks at how well the estimate fits to the data. The confidence region we can show in the above diagram is our confidence in the
(total) fit, which means that we are 95% certain that our estimate will fall in the indicated region. 
If we smooth the input data, our fit increases, because the standard deviation has decreased. 

<h4>Fit</h4><p>Statistics knows a number of methods to calculate how good the regression line fits
the data. The classical methods do not work so well for time series. I have chosen to measure it by Allen's 
predictive R squared (1974). See ... This method penalizes for the use of too many factors.</p> 

<h4>Confidence Level</h4><p>The global temperature time series is a so called autocorrelated time series. That means that the tempartures of this month depend to some extend of the
temperature of last month. The regression does not take that into account and therefor calculates the wrong
confidence interval. We calculate a corrected value, that is visible when you click the check box. 
The confidence decreases dramatically is you increase the smooting of the data! The reason is that the smoothed data has a high degree of auto correlation, and the 
confidence interval is corrected for autocorrelation. </p> 
"

tutorial3.1=
 "<h4>Solar Irradiation</h4><p>The sun shines down on the earth with tremendous force
, but that strength varies slightly over time. Only with satellites we have started to be 
able to measure the variations in the strength
of the sun's radiation - so there are only recent records. Sunspots though seem to indicate more radiation from the sun, that's
why we can use the sunspot records that have been collected for hundreds of years.</p><p> 
 In the following diagram check the box for the sun and see what happens.</p><hr>"

tutorial3.2=
 "<hr><h4>Volcanoes</h4><p>Volcanoes can temporarily cool the planet. Big outbreaks have caused some
summers to be very cool. The particle that cause the cooling are called aerosols. Check the box for volcanoes 
to see the impact on the temperature.</p><h4>Human Aerosols</h4>
<p>At this moment the data about the emission of aerosols by humans is included in the volcanic data. 
 I try to find better sources for the data and will include them later. </p>
<h4>Day Lengths</h4><p>Other interesting phenomena are the very small changes in the length of the day (LoD).
 Although these changes are in milliseconds per year, 
they are signs of considerable releases of energy some 7 years before the changes in the daylength occur.</p>"

tutorial4.1=
 "<p> Many ocean currents influence the local weather and some have a global impact. 
Let's look at three of them.</p><hr>"

tutorial4.2=
 "<hr>
<h4>El Nino and Atlantic Multidecadel Oscillation</h4><p>First check these boxes and you will see that 
 there is quite an impact. ENSO stands for the El Nino Southern Oscillation. El Nino is a phenomenon
 that happens every four or five years when lots of warmer water from the deep ocean surface
 before the West coast of South America. What is measured is the so called Southern Oscillation, 
 the difference in pressure between far away parts of the ocean.</p>
 <p>In the mid Atlantic the AMO also influences the weather. It is measured from the
 temperature, so it it a bit tricky[*]. First that needs to be removed. Then a little clear signal appears, 
but it is mainly similar to a little part of the Length of day signal.</p>
 <h4> Pacific Decadal Oscillation</h4><p>Less clear is the contribution of the PDO, in the mid Pacific. It is sometimes mentioned 
 as a possible cause for climate variations, but nothing can be found in this signal. In fact, the fit decreases a little bit if one turns the PDO signal on.</p>
<small><p>[*] I used the wrong dataset initially. That has been repaired 2014-12-02.</p></small>"
tutorial5.1=
 "<h4>Seasons</h4><p>The orbit of the earth around the sun is not a perfect circle, but an ellipse. 
 So the amount of sunshine we get changes a little bit over the year. Also, the Northern Hemisphere
 has much more land than the Southern, which influences the natural production of CO2.</p>
 "
tutorial5.2=
 "<h4>Planet Cycles</h4><p>There are some planetary cycles (look them up in Wikipedia!)
that have to do with the movement of the sun, moon and planets around the earth. Of course they influence the tides, but also the distances between the earth and sun, and others.
We have found five cycles that show (some small) correlation with temperature. Each separate shows a small contribution, but the total adds more
fit than the individual components. That is a sign that we might be overfitting. 
Do not use them if you have a smaller (57 years or less) regression period. 
The longest cycles are 18+ years long. It is a rule of thumb to have a regression period that spans at least 3 cycles.</p>"

tutorial6.1=
 "<p>Now it is your turn to apply what you have learned. On this page the most useful functions are combined. 
For all possibilities you need to go to the full applet. 
 </p><hr>"
tutorial6.2=
"<hr><p>Enjoy!</p>"
tutorial6.3=
  "<h4>Was there a pause? No!</h4><p>In our second example we zoom in on the last 17-18 years: from 1997 onwards.
Quite daring we use only that period for the regression, with smoothing of 18 months. All options have been
turned on now, except the PDO. 
We see a very good fit, and a TCR still close to 2. 
</p><hr>"
tutorial6.4=
  "<hr><p>Just use the slider to change the start year. Anywhere below 1997 the fit and the TCR stay
stable. Of course this is only an indication that the variability of the factors has masked the
warming by the CO2, and more details are needed to show that the effect is not due to chance. We can do that by inspecting the coefficients of the
regression: they stay similar. A stronger proof is that
we can actually predict this behaviour, using data from previous years only. 
This will be shown in a future version of this applet.</p>"


details_page1=
 "<p>NOTE: enso and soi yield nearly identical results. Sunspots and TSI also yield nearly identical results.At this moment we use the enso and TSI signals.</p>

 "

details_page2=" 

<h4>Mathematics</h4>
<p>The model used is basically a multilineair regression model. A similar approach
can be found in the literature[1,2]. The time series is composed of one trend and many natural variations. Those variations should be stationary time series. 
The trend component is a function of an external forcing (such as log(CO2 level) 
without (direct) autocorrelation, the rest are autocorrelated stationary series. As we do not know all variations the error term is also auto-correlated.
To calculate standard errors, that remainder is assumed to be ARMA(1,1) and we use the code from [2].
</p>

<p>Because of this model, natural variations with a longer pseudo-period cannot be used reliably on shorter series. In future versions of this applet a warning will be issued if this 
might be the case. </p>

<p>As measurement for the quality of the fit of the model I use
a lesser known function called the predictive R-squared [3]. It is more intuitive than the better known
AIC or BIC. It performs equally well, penalizing for more covariates. It comes from one of the pioneers of 
the cross validation approach for model selection.</p>

<p>There are several ways to show that the model actually works, and that the fit we see is not due to chance. 
In a future version of this applet I will show more of the technical details, for instance by making hindcasts and forecasts.</p>
<h5>References</h5><small>
 <p>[1] Lean & Rind (2009), How will Earth's surface temperature change in future decades<br />
[2] Foster & Rahmstorf (2011), Global temperature evolution 1979-2010<br />
[3] Allen, D. M. (1974), The Relationship Between Variable Selection and Data Augmentation and a Method for Prediction, Technometrics, 16, 125-127<br />
[4] <a href='http://contextearth.com/2013/10/26/csalt-model/'>http://contextearth.com/2013/10/26/csalt-model/</a></p>
</small>

<h4>R Sources</h4>
<p>The sources and data can be found <a href='https://github.com/MrOoijer/R-work'>in my Github. </a></p>
"