*****************************************************************************************
* 	weather                                                   			       	 	    *
*	v 1.0  17may2017	by	Oscar Barriga Cabanillas	- obarriga@ucdavis.edu		    *	
*	v 2.0  16mjul2017	by	Oscar Barriga Cabanillas	- obarriga@ucdavis.edu		    *	
*			New stuff done by Aleksandr Michuda 		- amichuda@ucdavis.edu		    *
*	v 3.0  2july2019	by  Jeffrey D. Michler			- jdmichler@email.arizona.edu   *
*   v 3.1  5july2019    by  Brian McGreal				- bmcgreal@email.arizona.edu    *
*   v 3.2  8july2019    by  Anna Josepshon				- aljosephson@email.arizona.edu *
*****************************************************************************************


pause on
cap program drop weather
program define weather  , eclass


* Define tempnames

version 15.1

	syntax anything 								///
		,											///
		ini_month(string)							///
		fin_month(string)							///
		[											///
		day_month(string)							///
		keep(string)								///
		save(string)								///
		growbase_low(real 0)						///
		growbase_high(real 0)						///		
		bins(real 5)								///
		temperature_data							///
		rain_data									///
		]											///
		


	
*0.0) If day is missing, it is assumed to be 01

if "'`day_month'" == "" {
	loc day_month = "01"
}


*0.3) Check options


if "`temperature_data'" == "temperature_data" {
	if `growbase_low' == 0 {
		di in red "Please define the temperature range to evaluate"
		error
	}
	if `growbase_high' == 0 {
		di in red "Please define the temperature range to evaluate"
		error
	}	
	if "`rain_data'" == "rain_data" {
		di in red "rain and temperature options cannot be used simultaneously"
		error
	}		
	
}


*1) loading variables to be use in the estimation


qui: ds , has(varlabel  `anything'*) alpha
qui: loc objective = r(varlist)

/*
*2) loading variables to be use in the estimation
	we get the variables that match certain name characteristics
	specified in the options of the command
*/

loc months = "01 02 03 04 05 06 07 08 09 10 11 12"
loc days = "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31"

* We need to know how many characters to substract from the loc(candidate) 
loc length_anything = length("`anything'")
loc length_anything = `length_anything ' + 1 

* This local will store the variables that go into the matrix
loc var = ""
loc safe2 = 1

* Help identify the first year that is used, so I can create a local with that name
loc count = 0
forvalues j = 1983(1)2017 {


	* Tempname for the matrix
	*tempname mat_`j'
	
	foreach month of loc months  {
		foreach day of loc days  {
			
			loc candidate = "`anything'`j'`month'`day'"
			
			loc candidate_year = substr("`candidate'", `length_anything' , 4)
			loc candidate_month = substr("`candidate'", `length_anything' + 4 , 2)
			loc candidate_day = substr("`candidate'", `length_anything' + 6 , 2)
			
			
			* We start selecting the variables that will be  use in the estimation
			* We run this until we reach  fin_month and  day_month again.
			* When reached we should stop adding info to the matrix			
			
			
			* WE only start until the variables start existing
			qui: cap confirm numeric variable `anything'`j'`month'`day'
			
			* To avoid entering the conditional of line 91 before the loop gets into a valid month thte first time
			loc safe = 0
				
			if _rc == 0 {
				loc go = 0 
			
			if `count' == 0 loc ini_year = "`j'"
			loc ++count
			
			
				if (`candidate_month' >= `ini_month') {
					if (`candidate_month' == `ini_month' & `candidate_day' >= `day_month' ) 		loc go = 1
					if (`candidate_month' > `ini_month' ) &	(`candidate_month' < `fin_month')		loc go = 1
					if (`candidate_month' == `fin_month') & (`candidate_day' <= `day_month' ) 		loc go = 1
					
						if `go' == 1 {		
						
							loc var = "`var' `anything'`j'`month'`day'"
							loc safe = 1
							loc safe2 = 0
						}
				}
			
			
				if (`candidate_month' >= `fin_month') & (`candidate_day' > `day_month') & (`safe' == 0 ) & (`safe2' == 0) {

				
					loc final_year = `j'
					 
					* At this point I calculate the statistics I want using vars in loc var
					
					* Mean
					qui: egen mean_season_`j' = rowmean(`var')
					label var mean_season_`j' "Season avg months: `ini_month' `fin_month' on `j'"

					* Median
					qui: egen median_season_`j' = rowmedian(`var')
					label var median_season_`j' "Season median months: `ini_month' `fin_month' on `j'"
					
					* sd
					qui: egen sd_season_`j' = rowsd(`var')
					label var sd_season_`j' "Season s.d months: `ini_month' `fin_month' on `j'"					
					
					* Total
					qui: egen total_season_`j' = rowtotal(`var')
					label var total_season_`j' "Season total months: `ini_month' `fin_month' on `j'"
					
					* skewness
					qui: gen skew_season_`j' = (mean_season_`j' - median_season_`j')/sd_season_`j' 
					label var skew_season_`j' "Season skew months: `ini_month' `fin_month' on `j'"					
					
					* max
					qui: egen max_season_`j' = rowmax(`var')
					label var max_season_`j' "Season max months: `ini_month' `fin_month' on `j'"
					
					
					* Number of days in the season
					loc count_days : word count  `var'
					
					
					* Some sats are only calculated for temperature data, but nor for rain
					
					if "`temperature_data'" == "temperature_data" {
						
						*growing degree days
						foreach f of local var {
							qui: gen aux_gd_`f' = inrange(`f' , `growbase_low' , `growbase_high')
						}
						
						
						qui: egen gdd_`j' = rowtotal(aux_gd_*)
						label var gdd_`j' "Number of growing degree days in `j' between `growbase_low' `growbase_high'"
						
						drop aux_gd_*
					
					
						* Schlenker/Roberts temperature bins
						forval i= 20(20)80 {
							qui: egen percentile`i'`j' = rowpctile(`var'), p(`i')
														
						}
						
									
						foreach f of local var {
							
							qui: gen aux20`f' = `f' < percentile20`j'
							qui: gen aux40`f' = inrange(`f' , percentile20`j' , percentile40`j')
							qui: gen aux60`f' = inrange(`f' , percentile40`j' , percentile60`j')
							qui: gen aux80`f' = inrange(`f' , percentile60`j' , percentile80`j')
							qui: gen aux100`f' = `f' > percentile80`j'
							
						}
						
						* percentage of days  of days on each bin	
						qui: egen tempbin20`j'  = rowtotal(aux20*) 
						qui: replace   tempbin20`j' =   tempbin20`j'/`count_days'
						
						qui: egen tempbin40`j'  = rowtotal(aux40*) 
						qui: replace   tempbin40`j' =   tempbin40`j'/`count_days'
						
						qui: egen tempbin60`j'  = rowtotal(aux60*) 
						qui: replace   tempbin60`j' =   tempbin60`j'/`count_days'
						
						qui: egen tempbin80`j'  = rowtotal(aux80*) 
						qui: replace   tempbin80`j' =   tempbin80`j'/`count_days'
						
						qui: egen tempbin100`j'  = rowtotal(aux100*)
						qui: replace   tempbin100`j' =   tempbin100`j'/`count_days'
						
						
						
						label var  tempbin20`j'  "The number of days in the  20th percentile of temperature in year `j'"
						label var  tempbin40`j'  "The number of days in the  40th percentile of temperature in year `j'"
						label var  tempbin60`j'  "The number of days in the  60th percentile of temperature in year `j'"
						label var  tempbin80`j'  "The number of days in the  80th percentile of temperature in year `j'"
						label var  tempbin100`j' "The number of days in the  100th percentile of temperature in year `j'"
						
						* Fix percetnages to a 100 due to imprecision on percentiles
						qui: egen aux_to100 = rowtotal( tempbin20`j'  tempbin40`j'  tempbin60`j' tempbin80`j' tempbin100`j' )

						qui: replace tempbin20`j'  = tempbin20`j'/aux_to100
						qui: replace tempbin40`j'  = tempbin40`j'/aux_to100						
						qui: replace tempbin60`j'  = tempbin60`j'/aux_to100						
						qui: replace tempbin80`j'  = tempbin80`j'/aux_to100						
						qui: replace tempbin100`j' = tempbin100`j'/aux_to100						
						
						qui: drop aux_to100 aux20* aux40*  aux60* aux80* aux100*
						
						
					
					}	
					
				if "`rain_data'" == "rain_data" {
				
					*days without rain
					foreach f of local var {
						qui: gen aux_norain_`f' = `f' < 1
					}
					
					*days without rain count					
					qui: egen norain_`j' = rowtotal(aux_norain_*)
					label var norain_`j' "Number of days without rain in `j'"
					
					*days with rain
					qui: gen raindays_`j' = `count_days' - norain_`j'
					label var raindays_`j' "Number of days with rain in `j'"
					
					*days with rain percent
					qui: gen raindays_percent_`j' = raindays_`j'/`count_days'
					label var raindays_percent_`j' "Percentage of days with rain in `j'"
					
					drop aux_norain_*
					
					*longest dry spell
					foreach f of local var {
						qui: gen aux_`f' = 0 if `f' == 0
						qui: replace aux_`f' = 1 if `f' > 0
					}
					
					qui: egen hist_`j' = concat(aux*)
					drop aux*
					qui: gen ssn_`j' = substr(hist_`j', strpos(hist_`j', "1"), .)
					qui: replace ssn_`j' = substr(ssn_`j', 1, strrpos(ssn_`j', "1"))
					*qui: gen ssn_lngth_`j' = strlen(ssn_`j')
					
					gen dry_`j'  = 0
					label var dry_`j' "Longest intra-season dry spell in `j'"		
					
					*local ssn_lngth ssn_lngth_*
					local lookfor : di _dup(`count_days') "0" // change count_days to lenght of ssn
					qui forval k = 1/`count_days' { 
					replace dry_`j' = `k' if strpos(ssn_`j', substr("`lookfor'", 1, `k')) 
					} 
 				}

					* Cleans the loc var so it can strat again from zero and updates the dafe2 local indicatig that a new round of vars is going to be collected
					loc var = ""
					loc safe2 = 1
				
				}					
			
			}
				
		}
	}
}		


***********
* Now we create deviations from the seasons
***********
if "`rain_data'" == "rain_data" {

loc deviation = "total_season raindays norain raindays_percent" 

foreach var of loc deviation {

	* gen average of the seasons - keep this
	qui: egen mean_period_`var' = rowmean(`var'_*)
	label var mean_period_`var' "Average of `var' between seasons in months: `ini_month' `fin_month' on years `ini_year' `final_year'"

	* gen sd of the seasons - keep this
	qui: egen sd_period_`var' = rowsd(`var'_*)
	label var sd_period_`var' "SD of `var' between seasons in months: `ini_month' `fin_month' on years `ini_year' `final_year'"
	

	* Z-scores - keep this
	forvalues j = 1983(1)2017{

		* We only start until the variables start existing
		qui: cap confirm numeric variable `var'_`j'
		
		if _rc == 0 {
			qui: gen dev_`var'_`j' = `var'_`j' - mean_period_`var'
			label var dev_`var'_`j' "Deviation in `var' from long-run average"
		
			qui: gen z_`var'_`j'  = (`var'_`j'-mean_period_`var')/sd_period_`var'
			label var z_`var'_`j' "Z-score of `var'  between seasons in months: `ini_month' `fin_month' on years `ini_year' `final_year'"

		}	
		
	}
	
}

}

***********
* Main Temperature Statistics
***********
if "`temperature_data'" == "temperature_data" {
*** GDD deviations from LR mean - keep this
	* gen average annual number of growing degree days
	qui: egen mean_gdd = rowmean(gdd_*)
	label var mean_gdd "Long Term growing degree days between  `growbase_low' `growbase_high' seasons"
	
	* gen sd of growing degree days - keep this
	qui: egen sd_gdd = rowsd(gdd_*)
	label var sd_gdd "Standard Deviation of growing days between seasons between  `growbase_low' `growbase_high'"
	

	* Z-scores - keep this
	forvalues j = 1983(1)2017{

	* We only start until the variables start existing
		qui: cap confirm numeric variable gdd_`j'
		
		if _rc == 0 {
			qui: gen dev_gdd_`j' = gdd_`j' - mean_gdd
			label var dev_gdd_`j' "Deviation in GDD from long-run average"
		
			qui: gen z_gdd_`j'  = (gdd_`j'-mean_gdd)/sd_gdd
			label var z_gdd_`j' "Z-score of GDD between seasons in months: `ini_month' `fin_month' on years `ini_year' `final_year'"

		}	
		
	}
	
	forval k = 20(20)100 {
	
		qui: egen mean_`k' = rowmean(tempbin`k'*)
		label var mean_`k' "Average percentage number of days in the `k'th percentile between all seasons"
		
		qui: egen sd_`k' = rowsd(tempbin`k'*)
		label var sd_`k' "SD of the percentage of number of days in the `k'th percentile between all seasons"
		
	}
}


***********
* Main Rainfall Statistics
***********

*** No rainfall deviations from the LR mean - keep this

* gen average annual number of days with no rain
*not using currently - 8 July 2019
/*

if "`rain_data'" == "rain_data" {

	qui: egen mean_norain = rowmean(norain_per*)
	label var mean_norain "Average number of days without rain between seasons"

	qui: egen sd_norain = rowsd(norain_per*)
	label var sd_norain "SD of days without rain between seasons"


}
*/

* We keep only what we need if the option keep was used
if "`keep'" != "" {
	if "`rain_data'" == "rain_data" {
		di in r "option keep was chosen"
		qui: keep `keep' *season*  *norain* *raindays* dry*
		qui: drop z_raindays* z_norain* max*
		

	}
	
	if "`temperature_data'" == "temperature_data" {
		qui: keep `keep' *season* *gdd* tempbin* 
		qui: drop total_season_*

	}
}

if "`save'" != "" {


	di in y "Saving data set as `save'"
	save "`save'" , replace
}

end
// ------------------------------------------------------------------

exit




