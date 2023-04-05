// New DRDID

* Let's first install drdid
ssc install drdid, all replace
* Now let's install csdid
ssc install csdid, all replace

* Help file for csdid
help csdid
* Help file for Post-estimation procedures associated with csdid
help csdid_postestimation

clear 
cd "C:\Users\darra\Dropbox\UCD PhD\PhD\Econo 2\project"
import excel "C:\Users\darra\Dropbox\UCD PhD\PhD\Econo 2\project\DF2.xlsx", sheet("Sheet1") firstrow

csdid repshare stateyear mspost  i.year##popsd , ivar(county) time(year) gvar(treatedfirst) notyet dripw

csdid repshare stateyear mspost  i.year##popsd , ivar(county) time(year) gvar(treatedfirst) asinr  dripw

  csdid_plot