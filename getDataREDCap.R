
#Question 1

#download data
# get the data directly from redcap:
# report #155 is Q1:

url <- "https://redcap.ispm.unibe.ch/api/"
token <- "################################"
formData <- list("token"=token,
                 content='report',
                 format='csv',
                 report_id='155',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='csv'
)
