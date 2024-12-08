rm(list = ls())
library("rmarkdown")

#In this script I am calling the render function of Rmd in a loop
#In each one I define v which will be the identifier of the stakeholder
#so that I am reading different parts of df1, df2 etc. In this way
#I can make personalized plots, lists of names etc. 

ids = read.csv("id_sites.csv")

for(v in 1:10){#Ideally it would be n (=50), number of reports to do.
  render("Template_for_loop.Rmd",
         output_file=paste0("Report_", ids$id[v], ".pdf"),
         #Define the name of the report
         params=list(new_title=paste("Report of field", ids$id[v], paste = " ")))
#This is for the YAML so that it can change the title according to whatever you want
}

#In the Template_for_loop.Rmd we read the data.frames and in each part
#of the report we are calling for those corresponding to the target
#stakeholder (called by the "v" parameter from the loop)