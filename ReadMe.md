# They Struck for Higher Pay: A Case Study of Teacher Retention After a \$6,000 Pay Raise in Oklahoma

## Directory Info
* figures: All figures included in the paper
* R 
  * 0_data-cleaning: Scripts to process raw data and create files found in `data/` directory. 
  * 1_analysis:Scripts to run analysis, figures/tables created indicated in code comments.
* data-raw: Includes all original source data. Not available on github due to file size. Available
in [OSF repository](https://osf.io/7f6ms/files/). Note that the Texas data needs to be 
unzipped. 
  * Kansas data obtained by public records request, not updated and omitted from EFP version 
  * Oklahoma data downloaded from [Oklahoma Department of Education](http://sde.ok.gov/sde/documents/2018-01-02/certified-staff-salary-information)
  * Texas data downloaded from Texas Education Agency: 
    * [Snapshot](https://rptsvr1.tea.texas.gov/perfreport/snapshot/download.html)
    * [APR Reports](https://tea.texas.gov/Student_Testing_and_Accountability/Accountability/State_Accountability/Performance_Reporting/Texas_Academic_Performance_Reports)
  * NCES data downloaded from [elsi table generator](https://nces.ed.gov/ccd/elsi/tableGenerator.aspx)
  * Border counties file hand created by author using google maps. 
* data: Includes all cleaned data. Scripts to create all files in this directory are included 
in R/0_data-cleaning. `clean_district.rds` and `clean_state.rds` are the primary 
analysis files. 
