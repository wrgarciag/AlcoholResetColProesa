*********************
******************
*
* QAIDS stimation
*
******************
*********************

*log using "C:\Users\wgarcia\Universidad Icesi (@icesi.edu.co)\Proesa - *22-1002-PahoElasSsb\Resu\SSBElasticityPaper_log.log", append

clear all
macro drop _all
set more off


global pmain "H:\My Drive\Research\Proesa - 22-1002-PahoElasSsb"

cd "$pmain"

global pcode "$pmain\Code"
global pcodeM "$pcode\Model"
global path  "$pmain\Data\Output"
global presu "$pmain\Resu"
global prtab "$presu\Tab"

****************************
* Summary statistics
****************************
do "$pcode\SSBElasticityPaperDescribe.do"

****************************
* Off trade model
****************************
do "$pcode\SSBElasticityPaperRevised_hogar.do"

****************************
* On trade model
****************************

****************************
* Combined model
****************************


log close
