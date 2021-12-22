# file to store configuration 


################ set user data here: ###############

# example project
#API<- '4E0DB920DE37B84B5B905773scdfjkl' # not valid API key
#user<- 'MC' # for example KM
#reportid = '150' #screening report

# live project
reportid='152'
API='#################################'

####################################################


row_limit = 30

log_dir_veri = "logs_veri"
temp_dir_veri = "temp_veri"

log_dir_screen = "logs_screen"
temp_dir_screen = "temp_screen"


logins=read.csv(file="logins.csv", stringsAsFactors = FALSE)
