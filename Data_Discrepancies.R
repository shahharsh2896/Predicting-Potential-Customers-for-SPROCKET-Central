#Customer_Demo
summary(Customer_Demo)

#Validity
#The default column is not in readable format.
Customer_Demo$deceased_indicator<-as.factor(Customer_Demo$deceased_indicator)
Customer_Demo$owns_car<-as.factor(Customer_Demo$owns_car)
Customer_Demo$gender<-as.factor(Customer_Demo$gender)
# Does "F","Femal" & "Female" all represent the same gender Female or not, if yes
# they should all be having the same representation. Similar observation for "M"
# & "Male". Also if "U" denotes unknown gender, it should be mentioned as 
# "Unknown". The data should be consistent either M/F/U or Male/Female/Unknown.

#Completeness
table(is.na(Customer_Demo))
(1045/50955)*100
#2.050829%
#Overall 1045 NA
table(is.na(Customer_Demo$tenure))
100*87/1045
#8.325359%
#87 NA
table(is.na(Customer_Demo$last_name))
125*87/1045
#125 NA
#10.4067%
table(is.na(Customer_Demo$DOB))
100*87/1045
#87 NA
#8.325359%
table(is.na(Customer_Demo$job_title))
100*506/1045
#506 NA
#48.42105%
table(is.na(Customer_Demo$default))
100*240/1045
#240
#22.96651%

table(duplicated(Customer_Demo$customer_id))
#No Duplicates

cust <- as.numeric(substring(as.character(Customer_Demo$customer_id), 1))
cust_min <- as.numeric(min(cust))
cust_max <- as.numeric(max(cust))
omit <- as.data.frame(setdiff(cust_min:cust_max, cust))
n <- nrow(omit)
om_percent_cust<-100*(n/nrow(Customer_Demo))
cat("\n # of omitted customer id = ", om_percent_cust)
#No Omissions

#Completeness
#Demographics of Customer_ID 4001, 4002, 4003 are not available.


###########################################################################

summary(Cust_Address)
table(is.na(Cust_Address))
table(duplicated(Cust_Address$customer_id))

#Completeness
cust <- as.numeric(substring(as.character(Cust_Address$customer_id), 1))
cust_min <- as.numeric(min(cust))
cust_max <- as.numeric(max(cust))
omit <- as.data.frame(setdiff(cust_min:cust_max, cust))
n <- nrow(omit)
#4
om_percent_cust<-100*(n/nrow(Cust_Address))
cat("\n # of omitted customer id = ", om_percent_cust)
#0.1
#Adresses of Customer_ID 10, 22, 23 are missing in the dataset.

###########################################################################

summary(Transactions)
Transactions$transaction_date<-as.character(Transactions$transaction_date)
Transactions$online_order<-as.factor(Transactions$online_order)
Transactions$order_status<-as.factor(Transactions$order_status)
Transactions$product_line<-as.factor(Transactions$product_line)
Transactions$product_class<-as.factor(Transactions$product_class)
Transactions$product_size<-as.factor(Transactions$product_size)
Transactions$brand<-as.factor(Transactions$brand)

#Consistency
# Customer_ID 5034 is not consistent with other datasets. Probably it's a misprint. 

#Completeness
table(is.na(Transactions))
100*(1542/258458)
# 0.5966153%
# Total 1542 NA
# 197 rows not having values for Brand, product_line, product_class, 
# product_size, standard_cost, product_first_sold_date.
# which totals 197*6 that is 1182 NA

table(is.na(Transactions$online_order))
# 360 NA

#Uniqueness
table(duplicated(Transactions$customer_id))
#16506
table(duplicated(Transactions$product_id))
#19899
table(duplicated(Transactions$transaction_id))
#0

#Completeness
tran <- as.numeric(substring(as.character(Transactions$transaction_id), 1))
tran_min <- as.numeric(min(tran))
tran_max <- as.numeric(max(tran))
omit <- as.data.frame(setdiff(tran_min:tran_max, tran))
n <- nrow(omit)
#0
