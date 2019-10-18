/*===========================================================================
project:      Weather Statistics 
Author:       Anna Josephson & Jeffrey D. Michler
---------------------------------------------------------------------------
Creation Date:      July  16, 2017 
Adapted Date:		July  11, 2019
===========================================================================*/

clear all
set more off
set maxvar 120000

discard

/*=========================================================================
                         0: Program Setup
===========================================================================*/

*-----0.1: Set up directories

global user "jdmichler"

* For data
loc root = "C:\Users/$user\Dropbox\Weather_Project\Data\"
* To export results
loc export = "C:\Users/$user\Dropbox\Weather_Project\Results\"


/*=========================================================================
                         1: Run command for rain datasets
===========================================================================*/

use "`root'\ihs4p_x0_rf1_daily" , clear

weather rf_ ,  rain_data ini_month(3) fin_month(5) day_month(15) keep(y3_hhid) save("`export'/results_rain")


/*=========================================================================
                         2: Run command for temperature datasets
===========================================================================*/

use "`root'\ihs4p_x0_t1_daily" , clear

*Note temperatures are in Celsius
weather y_ , temperature_data growbase_low(10)  growbase_high(30) ini_month(3) fin_month(5) day_month(15) keep(y3_hhid) save("`export'/results_temp")






