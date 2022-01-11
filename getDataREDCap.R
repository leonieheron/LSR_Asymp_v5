
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


# get the ROB data directly from redcap:
urlrob <- "https://redcap.ispm.unibe.ch/api/"
#APIs token is not sharable
token <- "##################################"
formDatarob <- list("token"=tokenrob,
                    content='report',
                    format='csv',
                    report_id='283',
                    csvDelimiter='',
                    rawOrLabel='raw',
                    rawOrLabelHeaders='raw',
                    exportCheckboxLabel='false',
                    returnFormat='csv'
)
