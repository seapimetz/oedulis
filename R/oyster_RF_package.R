#' Transform Ostrea edulis wet weights to dry weights.
#'
#' @param x A vector including the wet weight to be transformed.
#' @param weight The type of weight to be transformed. The current version allows one of three options: a) Total weight ('total'); b) Shell weight ('shell'), and; c) Soft tissue weight ('tissue').
#'
#' @description
#' A simple way of transforming available weight data for the European flat oyster (Ostrea edulis). The transformation factors were published in Pineda-Metz et al. (2023), but will be updated when new data is available.
#' Note that the current version of the function only allows for input in vector form rather than a more complex input type (updates should improve this).
#' @examples
#' library(oedulis)
#' #Oyster data from Pineda-Metz et al. 2022
#' data(oyster)
#' #Test with total wet weight data
#' tww=oyster$Total_Wet_Weight
#'
#' ww_dw(x=tww,weight='total')
#'
#' @author Santiago E.A. Pineda-Metz
#' @references
#' Pineda-Metz SEA, Merk V, Pogoda B (2023) A machine learning model and biometric transformations to facilitate European oyster monitoring. Aquat Conserv 33(7), 708-720. https://doi.org/10.1002/aqc.3912
#' @import randomForest
#' @importFrom stats predict
#' @export
ww_dw=function(x,weight){
  if(weight!='total' & weight!='shell' & weight!='tissue') stop('Non valid argument for weight. Choose between total, shell or tissue')
  if(weight=='total'){ ww_dw=x*0.662 }
  if(weight=='shell'){ ww_dw=x*0.718 }
  if(weight=='tissue'){ ww_dw=x*0.176 }
  return(ww_dw)
}



#' Estimate Ostrea edulis wet or dry weights (both in g) from size data.
#'
#' @param x A vector or matrix including at least data on oyster length (in mm), depending on the model used, it will be necessary to also include data on oyster height (for models 2 and 3) and width (for model 3). Check description for more details.
#' @param type The type of weight to be transformed. The current version allows one of three options: a) Total weight ('total'); b) Shell weight ('shell'), and; c) Soft tissue weight ('tissue').
#' @param model The model to be used to predict oyster weight. The current version has 3 models: 1) Based on oyster length; 2) based on oyster length and height, and ; 3) based on oyster length, height and width.
#' @param weight Parameter to define if the weight to be estimated has to be wet weight ('wet') or dry weight ('dry'). Check description for more details.
#'
#' @description
#' An application of the models originally developed by Pineda-Metz et al. (2023) for estimating total, shell and soft tissue wet weights of European flat oyster (Ostrea edulis) based on size data.
#'
#' The models are based in the random forest algorithm developed by Breiman (2001). From the original models of Pineda-Metz et al. (2023) only the one for estimating soft tissue is as originally developed, whereas those for estimating total and shell wet weights are updated versions. The models have the following mean relative errors:
#' - Model 1 / 2 / 3 for Total Weight = 58.6% / 33.1% / 29.6%
#' - Model 1 / 2 / 3 for Shell Weight = 57.9% / 54.7% / 52.6%
#' - Model 1 / 2 / 3 for Soft Tissue Weight = 19.6% / 16.6% / 15.3%
#'
#' When using the formula, check that the names of the columns/vectors from which length, width and height data will be extracted are noted as 'Length', 'Width' and 'Height' respectively.
#'
#' Note that the definitions of oyster length, width and height do not follow the original description provided by Pineda-Metz et al. (2023), but the following:
#' - Length: Umbo hinge to longest edge.
#' - Width: Longest distance across the valve.
#' - Height: Maximum distance between external surfaces of the umbo.
#' @author Santiago E.A. Pineda-Metz
#' @references
#' Breiman L (2001) Random forests. Mach Learn 45, 5-32
#'
#' Pineda-Metz SEA, Merk V, Pogoda B (2023) A machine learning model and biometric transformations to facilitate European oyster monitoring. Aquat Conserv 33(7), 708-720. https://doi.org/10.1002/aqc.3912
#' @examples
#' # Import oyster biometric data
#' data(oyster)
#'
#' #Estimate oyster total wet weight from model based on length data only
#' o_edulis_w(oyster,model=1,type='total',weight='wet')
#'
#' @import randomForest
#' @import stats
#' @export
o_edulis_w = function(x,model,type,weight){
  warning('Remember that, length = distance from umbo hinge to longest edge, width = Longest distance across the valve, and height = Maximum distance between external surfaces of the umbo')
  if(model==1 & type=='total' & weight=='wet') {o_edulis_w=round(predict(length_WW,x),2)}
  if(model==2 & type=='total' & weight=='wet') {o_edulis_w=round(predict(length_height_WW,x),2)}
  if(model==3 & type=='total' & weight=='wet') {o_edulis_w=round(predict(size_WW,x),2)}
  if(model==1 & type=='shell' & weight=='wet') {o_edulis_w=round(predict(length_height_S_WW,x),2)}
  if(model==2 & type=='shell' & weight=='wet') {o_edulis_w=round(predict(length_S_WW,x),2)}
  if(model==3 & type=='shell' & weight=='wet') {o_edulis_w=round(predict(size_S_WW,x),2)}
  if(model==1 & type=='tissue' & weight=='wet') {o_edulis_w=round(predict(length_B_WW,x),2)}
  if(model==2 & type=='tissue' & weight=='wet') {o_edulis_w=round(predict(length_height_B_WW,x),2)}
  if(model==3 & type=='tissue' & weight=='wet') {o_edulis_w=round(predict(size_B_WW,x),2)}
  if(model==1 & type=='total' & weight=='dry') {o_edulis_w=round(predict(length_WW,x),2)*0.662}
  if(model==2 & type=='total' & weight=='dry') {o_edulis_w=round(predict(length_height_WW,x),2)*0.662}
  if(model==3 & type=='total' & weight=='dry') {o_edulis_w=round(predict(size_WW,x),2)*0.662}
  if(model==1 & type=='shell' & weight=='dry') {o_edulis_w=round(predict(length_S_WW,x),2)*0.718}
  if(model==2 & type=='shell' & weight=='dry') {o_edulis_w=round(predict(length_height_S_WW,x),2)*0.718}
  if(model==3 & type=='shell' & weight=='dry') {o_edulis_w=round(predict(size_S_WW,x),2)*0.718}
  if(model==1 & type=='tissue' & weight=='dry') {o_edulis_w=round(predict(length_B_WW,x),2)*0.176}
  if(model==2 & type=='tissue' & weight=='dry') {o_edulis_w=round(predict(length_height_B_WW,x),2)*0.176}
  if(model==3 & type=='tissue' & weight=='dry') {o_edulis_w=round(predict(size_B_WW,x),2)*0.176}
  return(o_edulis_w)
  }
