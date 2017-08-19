# libraries
library(rgl)
library(car)



# Tuning Parameters set-1
noOfCustomers = 2000
noOfVendors = 3
capacityPerVendor=500
customer_expectation= 8
vendor_expectation = 11
minimumPriceFactor = 0.4
maximumPriceFactor = 2

set.seed(1)
# ------------------------------Customer Data frame ----------------------------
customer_Df_static <- data.frame(id = 1:noOfCustomers,
                          income = rnorm(noOfCustomers,1000,200),
                          sex=rbinom(noOfCustomers, 1, 0.5),
                          p_clean=rnorm(noOfCustomers,6,2),
                          p_distance=rnorm(noOfCustomers,5,2),
                          p_f1=rnorm(noOfCustomers,6,2),
                          p_f2=rnorm(noOfCustomers,6,2),
                          p_f3=rnorm(noOfCustomers,6,2),
                          p_f4=rnorm(noOfCustomers,6,2),
                          p_f5=rnorm(noOfCustomers,6,2),
                          email_sensitivity = rnorm(noOfCustomers, 6, 2),
                          campus_event_sensitivity = rnorm(noOfCustomers, 6, 2),
                          social_sensitivity = rnorm(noOfCustomers, 6, 2),
                          social_factor= rnorm(noOfCustomers,6,2),
                          time_factor = rnorm(noOfCustomers,5,4),
                          expected_discount = rnorm(noOfCustomers, 50, 10),
                          status = 0,
                          signed_in_marktng_phase=0,
                          contract_price=0)

customer_Df_static$price_factor_1 = (customer_Df_static$p_clean + customer_Df_static$p_f1 + 
                                customer_Df_static$p_f2 + customer_Df_static$p_f3 + customer_Df_static$p_f4 +
                                customer_Df_static$p_f5) * customer_expectation

# ------------------------------- Vendor Data frame -------------------------------
vendor_Df_static <- data.frame(id = 1:noOfVendors,
                        v_clean=rnorm(noOfVendors,8,1),
                        v_distance=rnorm(noOfVendors,8,2),
                        v_f1=rnorm(noOfVendors,8,1),
                        v_f2=rnorm(noOfVendors,8,1),
                        v_f3=rnorm(noOfVendors,8,1),
                        v_f4=rnorm(noOfVendors,8,1),
                        v_f5=rnorm(noOfVendors,8,1),
                        capacity = capacityPerVendor,
                        emailcost=0,
                        social_event_cost = 0,
                        target_fraction_per_phase= c(1,1,1),
                        starting_price_fraction = c(1,1,1),
                        filled = 0,
                        total_revenue=0)

vendor_Df_static$price_factor_1 = (vendor_Df_static$v_clean + vendor_Df_static$v_f1 + 
                              vendor_Df_static$v_f2 + vendor_Df_static$v_f3 + vendor_Df_static$v_f4 +
                              vendor_Df_static$v_f5) *vendor_expectation

vendor_Df_static$minimum_price = vendor_Df_static$price_factor_1*minimumPriceFactor
vendor_Df_static$maximum_price = vendor_Df_static$price_factor_1*maximumPriceFactor

vendor_Df_static$Current_price = vendor_Df_static$price_factor_1* vendor_Df_static$starting_price_fraction
vendor_Df_static$Current_price_calculated = vendor_Df_static$Current_price


#*******************************************************************************************
#--------------------------------Tuning Parameters set-2------------------------------------------

best_rent_Ratio = 1
email_points_factor=5
event_points_factor=5
campaign_points_reduced_per_phase = 20
residual_campaign_points = 100
minimum_awareness_points=70
maxFractionOfImcomeForRent = 0.75
noOfCustPerDay=300


noOfMarketingPhases=15
#Sensitivity analysis parameters
startingPriceRange<-seq(from=1, to=1, by=1)
priceUpdateFactor<- seq(from=1, to=1, by=1)


#***************************************************************************************************
#***************************************************************************************************
#--------------------------------- Sensitivity Analysis Record -------------------------------------
#***************************************************************************************************


Sensitivity <- data.frame(startingPrice = rep(0, length(startingPriceRange)*length(priceUpdateFactor)),
                          priceUpdate= rep(0, length(startingPriceRange)*length(priceUpdateFactor)),
                          revenue = 0)
                        
# Dataframe with all possible combinations of startingPrice and PriceUpdate
kkk=1
for(iii in 1:length(startingPriceRange)){
  for(jjj in 1:length(priceUpdateFactor)){
    Sensitivity[kkk,1] = startingPriceRange[iii]
    Sensitivity[kkk,2] = priceUpdateFactor[jjj]
    kkk=kkk+1
  }
}

  
for(case in 1:nrow(Sensitivity)){
  
    vendor_Df<-vendor_Df_static
    customer_Df<-customer_Df_static
  
  
    vendor_Df$starting_price_fraction[1] = Sensitivity[case,1]
    vendor_Df$Current_price[1]= vendor_Df$price_factor_1[1] * Sensitivity[case,1]
    vendor_Df$Current_price_calculated = vendor_Df$Current_price
    
    vendor_Df$target_fraction_per_phase[1] = Sensitivity[case,2]
  


    #------------------------------- Customer Vendor relations ----------------------
    # Data frame for recording parameters connecting customers and vendors.
    ##
    customer_vendor_connection <- data.frame(id = 1:noOfCustomers, 
                                             v1_email = 0,v2_email = 0,v3_email = 0,
                                             v1_event = 0,v2_event = 0,v3_event = 0,
                                             v1_compatibility_score = 0,v2_compatibility_score = 0,v3_compatibility_score = 0,
                                             v1_social=0, v2_social=0, v3_social=0,
                                             timing_ratio=0,
                                             v1_rentRatio=0, v2_rentRatio=0, v3_rentRatio=0)
    
    
    
    #-----------------------------Changes with time-------------------------------------
    # Data frame for recording vendor prices with time
    phase_wise_vendor_status <- data.frame(phase_No = 1:noOfMarketingPhases,
                                           vendor1_price=0,
                                           vendor2_price=0,
                                           vendor3_price=0,
                                           vendor1_filled=0,
                                           vendor2_filled=0,
                                           vendor3_filled=0,
                                           vendor1_revenue=0,
                                           vendor2_revenue=0,
                                           vendor3_revenue=0)
    
    #--------------------- Marketing phases-----------------------------------------------
    for(t in 1:noOfMarketingPhases){
      
        ##------------------- Advertizement effect----------------------------------------------------------------------------------
        v1_email<-sample(1:noOfCustomers, noOfCustPerDay/2, replace = F)
        v2_email<-sample(1:noOfCustomers, noOfCustPerDay/2, replace = F)
        v3_email<-sample(1:noOfCustomers, noOfCustPerDay/2, replace = F)
        v1_event<-sample(1:noOfCustomers, noOfCustPerDay/2, replace = F)
        v2_event<-sample(1:noOfCustomers, noOfCustPerDay/2, replace = F)
        v3_event<-sample(1:noOfCustomers, noOfCustPerDay/2, replace = F)
        ##
        customer_vendor_connection$v1_email[v1_email]= customer_vendor_connection$v1_email[v1_email] + customer_Df$email_sensitivity[v1_email] * email_points_factor
        customer_vendor_connection$v2_email[v2_email]=customer_vendor_connection$v2_email[v2_email] + customer_Df$email_sensitivity[v2_email] * email_points_factor
        customer_vendor_connection$v3_email[v3_email]=customer_vendor_connection$v3_email[v3_email] + customer_Df$email_sensitivity[v3_email] * email_points_factor
        
        customer_vendor_connection$v1_event[v1_event]=customer_vendor_connection$v1_event[v1_event]+ customer_Df$campus_event_sensitivity[v1_event] * event_points_factor
        customer_vendor_connection$v2_event[v2_event]=customer_vendor_connection$v2_event[v2_event]+ customer_Df$campus_event_sensitivity[v2_event] * event_points_factor
        customer_vendor_connection$v3_event[v3_event]=customer_vendor_connection$v3_event[v3_event]+ customer_Df$campus_event_sensitivity[v3_event] * event_points_factor 
        
        
        #----------------------- Evaluation of each vendor by customers -------------------------------------------------------
        ## 
        customer_vendor_connection$v1_rentRatio <- customer_Df$price_factor_1/vendor_Df$Current_price[1]  
        customer_vendor_connection$v2_rentRatio <- customer_Df$price_factor_1/vendor_Df$Current_price[2] 
        customer_vendor_connection$v3_rentRatio <- customer_Df$price_factor_1/vendor_Df$Current_price[3] 
        
        #customer_vendor_connection$timing_ratio <- customer_Df$time_factor/t 
      
        for ( i in 1: noOfCustomers){
          for (j in 1:noOfVendors){
            #Compatability score
            customer_vendor_connection[i, j+7] <- sum(as.vector(vendor_Df[j,2:8]) * as.vector(customer_Df[i,4:10]))  
            #Social score
            if(i>2){
              customer_vendor_connection[i, j+10] <- ((customer_vendor_connection[i-1,j+7]) + 
                                                        customer_vendor_connection[i-2,j+7])*customer_Df$social_sensitivity[i]/10
            }else{
              customer_vendor_connection[i,j+10] = ((customer_vendor_connection[i+1,j+7]) + 
                                                      customer_vendor_connection[i+2,j+7])*customer_Df$social_sensitivity[i]/10
            }
          } 
        }
      
        customer_vendor_connection$v1_totalScore <- (customer_vendor_connection$v1_compatibility_score + customer_vendor_connection$v1_social + 
                                                     customer_vendor_connection$v1_email + 
                                                     customer_vendor_connection$v1_event)*customer_vendor_connection$v1_rentRatio
      
        customer_vendor_connection$v2_totalScore <- (customer_vendor_connection$v2_compatibility_score + customer_vendor_connection$v2_social + 
                                                     customer_vendor_connection$v2_email + 
                                                     customer_vendor_connection$v2_event)*customer_vendor_connection$v2_rentRatio
      
        customer_vendor_connection$v3_totalScore <- (customer_vendor_connection$v3_compatibility_score + customer_vendor_connection$v3_social + 
                                                     customer_vendor_connection$v3_email + 
                                                     customer_vendor_connection$v3_event)*customer_vendor_connection$v3_rentRatio
      
      
        # ---------------------Comparision and purchase of contract by each customers---------------------------------------
        marketingPhase_on_scale_10 <- t*10/noOfMarketingPhases
        for (i in 1: noOfCustomers){
          
         if((customer_Df$status[i]==0)&& (max(customer_vendor_connection[i,15:17])>1)){
           temp=which.max(customer_vendor_connection[i, c("v1_totalScore", "v2_totalScore","v3_totalScore")])
           if(customer_vendor_connection[i,temp+7]> quantile(customer_vendor_connection[,temp+7], 0.75) &&    #Compatibility
              ((customer_vendor_connection[i,temp+1] + customer_vendor_connection[i,temp+4]) > minimum_awareness_points ) &&   #Awareness
              (vendor_Df$Current_price[temp]< customer_Df$income[i]*maxFractionOfImcomeForRent)){   # Price
             
             customer_Df$status[i]=temp
             vendor_Df$filled[temp]=vendor_Df$filled[temp]+1
             vendor_Df$total_revenue[temp]= vendor_Df$total_revenue[temp] + vendor_Df$Current_price[temp]
             customer_Df$contract_price[i] = vendor_Df$Current_price[temp]
             customer_Df$signed_in_marktng_phase[i] = t
              phase_wise_vendor_status[t, 7+temp]= phase_wise_vendor_status[t, 7+temp] +
                                                  vendor_Df$Current_price[temp]
              
           }
         }
          if((customer_Df$status[i]==0) && ((customer_Df$time_factor[i]<marketingPhase_on_scale_10) || 
                                            (marketingPhase_on_scale_10>8))||(max(customer_vendor_connection[i,15:17])>1)){
            temp=which.max(customer_vendor_connection[i, c("v1_totalScore", "v2_totalScore","v3_totalScore")])
            if((vendor_Df$filled[temp]<vendor_Df$capacity[temp])&& 
               ((vendor_Df$Current_price[temp]< customer_Df$income[i]*maxFractionOfImcomeForRent))){
              customer_Df$status[i]=temp
              vendor_Df$filled[temp]=vendor_Df$filled[temp]+1
              vendor_Df$total_revenue[temp]= vendor_Df$total_revenue[temp] + vendor_Df$Current_price[temp]
              customer_Df$contract_price[i] = vendor_Df$Current_price[temp]
              customer_Df$signed_in_marktng_phase[i] = t
              phase_wise_vendor_status[t, 7+temp]= phase_wise_vendor_status[t, 7+temp] +
                vendor_Df$Current_price[temp]
            }
          }
        }
      
        # ------------------- Phase wise status of vendors ----------------------------------------------------------
        phase_wise_vendor_status$vendor1_price[t]=vendor_Df$Current_price[1]
        phase_wise_vendor_status$vendor2_price[t]=vendor_Df$Current_price[2]
        phase_wise_vendor_status$vendor3_price[t]=vendor_Df$Current_price[3]
      
        phase_wise_vendor_status$vendor1_filled[t]=vendor_Df$filled[1]
        phase_wise_vendor_status$vendor2_filled[t]=vendor_Df$filled[2]
        phase_wise_vendor_status$vendor3_filled[t]=vendor_Df$filled[3]
        
        if(t>1){
          phase_wise_vendor_status$vendor1_revenue[t]=phase_wise_vendor_status$vendor1_revenue[t]+
                                      phase_wise_vendor_status$vendor1_revenue[t-1]
          phase_wise_vendor_status$vendor2_revenue[t]=phase_wise_vendor_status$vendor2_revenue[t]+
                                      phase_wise_vendor_status$vendor2_revenue[t-1]
          phase_wise_vendor_status$vendor3_revenue[t]=phase_wise_vendor_status$vendor3_revenue[t]+
                                      phase_wise_vendor_status$vendor3_revenue[t-1]
        }

        # -------------------Price update by Vendors-----------------------------------------------------------------
      
        vendor_Df$expected_fillup = (vendor_Df$capacity * vendor_Df$target_fraction_per_phase * t / noOfMarketingPhases)

        vendor_Df$Current_price_calculated = ifelse(vendor_Df$filled<vendor_Df$capacity, (vendor_Df$price_factor_1*
                                                                                            (((vendor_Df$filled - vendor_Df$expected_fillup)/vendor_Df$capacity)+1)),
                                                    vendor_Df$Current_price_calculated)

        vendor_Df$Current_price = ifelse(vendor_Df$Current_price_calculated>vendor_Df$minimum_price, vendor_Df$Current_price_calculated,
                                         vendor_Df$minimum_price)

        vendor_Df$Current_price = ifelse(vendor_Df$Current_price>vendor_Df$maximum_price, vendor_Df$maximum_price,
                                         vendor_Df$Current_price)
        
        # Vendor-1 price should not exceed other vendors by 30%
        mn = min(vendor_Df$Current_price[2], vendor_Df$Current_price[3])
        mx = max(vendor_Df$Current_price[2], vendor_Df$Current_price[3])
        
        if(vendor_Df$Current_price[1]>= 1.3*mx)
          vendor_Df$Current_price = mx
        
        if(vendor_Df$Current_price[1]<= 0.7*mn)
          vendor_Df$Current_price[1] = mn
        
      # ------------------- Reduction in awareness among customers per phase --------------------------------------
        
        for(i in 1:noOfCustomers){
          for(j in 1:noOfVendors*2){
            
            if(customer_vendor_connection[i,j+1] > residual_campaign_points + campaign_points_reduced_per_phase){
              customer_vendor_connection[i,j+1]  = customer_vendor_connection[i, j+1] - campaign_points_reduced_per_phase
            }  
          }
        }
       
    }
    
    Sensitivity$revenue[case]=vendor_Df$total_revenue[1]
    print(phase_wise_vendor_status)
    print(vendor_Df)
    print(Sensitivity[case,])
    
    case_id=paste("cusNo_", noOfCustomers,"_sp_", Sensitivity$startingPrice[case], 
                  "_pf_", Sensitivity$priceUpdate[case],".csv", sep="")
    
    write.csv(phase_wise_vendor_status, case_id)
    
    # ---------------------Reports and visualizations -------------------------------------------------------------
    
    Vendor_id=1
    
    par(mar=c(5,5,5,5))
    hd= paste("Start Price Factor: ", Sensitivity$startingPrice[case],
                "Target Fraction per phase: ", Sensitivity$priceUpdate[case])
    plot(phase_wise_vendor_status$phase_No, phase_wise_vendor_status[,4+Vendor_id]/vendor_Df$capacity[Vendor_id],
         xlab = "Marketing Phase", ylab="Fraction occupied", main = hd, ylim=c(0,1),
         pch=16, type = "b", col='blue', font.lab=2)
    
    
    par(new = T)
    plot(phase_wise_vendor_status$phase_No, phase_wise_vendor_status[,1+Vendor_id], pch=17, 
         axes=F, xlab=NA, ylab=NA, col='red',type='b', ylim=c(0,800),font.lab=2)
    axis(side = 4)
    mtext(side = 4, line = 3, 'Price', font=2)
    
    #legend(noOfMarketingPhases*0.75, 200, c("Fraction filled", "Price"), pch=c(16,17), col=c('blue','red'))
    
    print("-------------------------------------------")
    print(paste("Vendor: ", Vendor_id, " Statistics"))
    print("-------------------------------------------")
    
    print(paste("Fraction Filled: ", vendor_Df$filled[Vendor_id]/ vendor_Df$capacity[Vendor_id]))
    print(paste("No Filled: ", vendor_Df$filled[Vendor_id]))


}

#s<-scatterplot3d(Sensitivity$startingPrice, Sensitivity$priceUpdate, Sensitivity$revenue,
#              pch=16, highlight.3d = TRUE, type = "h", main="Sensitivity", angle=120)

#write.csv(Sensitivity, "Sensitivity.csv")

phase_wise_vendor_status=read_csv("C:/Users/Gowthams/Desktop/Practicum/Demand less than supply/cusNo_750_sp_0.7_pf_1.csv")

par(mar=c(5,5,5,5))

plot(phase_wise_vendor_status$phase_No, phase_wise_vendor_status$vendor1_filled/500,
       xlab = "Marketing Phase", ylab="Fraction occupied", ylim=c(0,1),
       pch=16, type = "b", font.lab=2, col = alpha('blue',0.3))


points(phase_wise_vendor_status$phase_No, phase_wise_vendor_status$vendor1_filled/500,
     xlab = "Marketing Phase", ylab="Fraction occupied", ylim=c(0,1),
     pch=16, type = "b", font.lab=2, col = alpha('blue',0.3))


par(new = T)
plot(phase_wise_vendor_status$phase_No, phase_wise_vendor_status$vendor1_price, pch=17, 
     axes=F, xlab=NA, ylab=NA,type='b', ylim=c(0,800),font.lab=2, col=alpha('red', 0.3))




axis(side = 4)
mtext(side = 4, line = 3, 'Price', font=2)






