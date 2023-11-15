# oedulis: Ostrea edulis monitoring tools

Before anything: How to install in R?

If you don't have the devtools package installed, then:

      install.packages("devtools") #In case you don't have devtools installed
      devtools::install_github("seapimetz/oedulis")

That's it, well done!

The package was developed to help estimation and transformation of weight data for European flat oyster (Ostrea edulis) restoration and conservation efforts. The base publication used to develop this package was Pineda-Metz et al. (2023; http://dx.doi.org/10.1002/aqc.3912). These authors calculated tranformation factors to calculate total, shell and soft tissue dry weight from wet weights (or vice versa). Additionally, the mentioned publication developed a set of models for estimating total, shell and soft tissue wet weight of O. edulis. The package includes (in its current version) only two functions:
    1) 'ww_dt': Used for transforming O.edulis wet weights to dry weights.
    2) 'o_edulis_w': Used for estimating total, shell and soft tissue wet or dry weight of O.edulis. While the models used for estimating total and shell weight are an updated verion of the original models of Pineda-Metz et al. (2023), the one for estimating soft tissue wet weight remains in its original iteration.

The estimation of total, shell and soft tissue wet weights of O. edulis is based on random forest predictive models. 

The original models were developed considering data collected up to 2022 (check it here https://doi.pangaea.de/10.1594/PANGAEA.949238). These include the following number of observations:
- Total Wet Weight: N = 1241 | NOTE: Full data set was used for training model, testing was done using the OOB subset generated by the randomForest function
- Shell Wet Weight: N = 260 | NOTE: Full data set was used for training model, testing was done using the OOB subset generated by the randomForest function
- Soft tissue Wet Weight: N = 130 | NOTE: Full data set was used for training model, testing was done using the OOB subset generated by the randomForest function

The updated models were last revised on the 25.09.2023 and were only made for total and shell wet weights, as no new observations for soft tissue wet weight were made since the original models were developed. The updated versions for estimating total and shell wet weight include the following number of observations:
- TWW: Training set N = 2726 | Test set N = 682 | Total N = 3408
- SWW: Training set N = 1629 | Test set N = 407 | Total N = 2036

Whenever the models are updated, they will be posted here under 'NEWS'.

# IMPORTANT:
I understand that there's a big discussion on the deffinitions of length, width, and height within the European flat oyster community. The models were constructed under the following convention for them, and data needs to be input accordingly to ensure no miss-estimation or -interpretation of model results occur:
  - Length: Distance from umbo hinge to longest edge.
  - Width: Longest distance across the valve.
  - Height: Maximum distance between external surfaces of the umbo.

# Examples:

For 'ww_dw':
- Load your wet weight data as a vector.
- E.g.: If you want to transform your total wet weight to dry weight:

      data(oyster) #Example data set with oyster biometric data
      df=oyster$Total_Wet_Weight #Weight data in vector for
      transformed_data=ww_dw(x=df, weight='total') #The command 'weight' helps you to choose in between the total, shell and soft tissue wet weight
      transformed_data

  - In case you have a matrix into which you want to already add the transformed data (using 'oyster' as base:
      oyster$Transformed_Total_Wet_to_Dry_weight=ww_dw(x=df, weight='total')

For 'o_edulis_w':
- Load your size data as a matrix. Note that the columns containing length, width, and height data need to be named 'Length', 'Width', and 'Height' respectivelly.
- E.g.: To estimate total wet weight using the model based on length only:

      load(oyster) #Example data set with oyster biometric data
      estimated_total_wet_weight=o_edulis_w(x=df,model=1,type='total',weight='wet')
      estimated_total_wet_weight

  - In case you have a matrix into which you want to already add the transformed data (using 'oyster' as base:
      oyster$Estimated_Total_Wet_Weight=o_edulis_w(x=df,model=1,type='total',weight='wet')
    
- The function allows for estimating dry weight too, change the input of 'weight' to 'dry'. The function will first estimate the desired wet weight and then apply the corresponding transformation factor to obtain the dry weight.
