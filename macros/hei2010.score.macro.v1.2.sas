/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*                 THE HEI-2010 SCORING MACRO                        */                                               
/*                  (hei2010.score.macro.sas)                        */
/*********************************************************************/
/*                     VERSION 1.2         04/22/2016                */
/*                                                                   */
/* differs from VERSION 1.1 in comments and variable labels only     */
/*                                                                   */
/* This HEI-2010 macro is to be used to calculate densities and      */
/* and HEI-2010 component and total scores.                          */
/*								     */
/* The macro requires an input dataset with variables for each of    */
/* the HEI-2010 components, noted below.                             */
/*								     */	
/* Users should first use the beans and peas (legumes) allocation    */
/* macro to distribute legumes appropriately to the protein and      */
/* vegetable components. See http://riskfactor.cancer.gov/tools/hei/ */
/* for further details.						     */
/*                                                                   */
/* The resulting dataset, which is named by the user, contains the   */
/* same variables as the supplied dataset, and creates 25 new        */
/* variables. These include:					     */
/*                                                                   */				    
/*   The densities (per 1000 kcal) or percent (of total calories)    */
/*    for each of the 12 HEI-2010 components.			     */
/*                                                                   */
/*   Scores for the 12 components of the HEI-2010.                   */
/*                                                                   */
/*   The total HEI-2010 score, which is the sum of the               */
/*    scores for the 12 components.                                  */
/*                                                                   */                                                                  
/* The syntax for calling the macro is:                              */
/*                                                                   */
/* %HEI2010                                                          */
/* (indat= ,kcal=,lv_total=,lbeangrn=,f_total=,wholefrt=,g_whl=,     */
/*    d_total=,lallmeat=,lseaplant=,monopoly=,sfat=,sodi=,G_NWHL=,   */
/*    EMPTYCAL10=,outdat=); 			                     */
/*                                                                   */
/*  where                                                            */
/*                                                                   */
/*   "indat"        * Specifies the dataset to be used.              */
/*                                                                   */
/*   "kcal"         * Specifies calorie amount.                      */
/*                                                                   */
/*   "lv_total"     * Specifies the MPED/FPED intake of total veg    */
/*                      after legume allocation.                     */
/*                                                                   */
/*   "lbeangrn"     * Specifies the MPED/FPED intake of dark green   */
/*                      veg and beans after legume allocation.       */
/*                                                                   */
/*   "f_fotal"      * Specifies the MPED/FPED intake of F_TOTAL.     */
/*                                                                   */
/*   "wholefrt"     * Specifies the MPED/FPED intake of whole fruit. */
/*                                                                   */
/*   "g_whl"        * Specifies the MPED/FPED intake of whole grain. */
/*                                                                   */
/*   "d_fotal"      * Specifies the MPED/FPED intake of total dairy. */
/*                                                                   */
/*   "lallmeat"     * Specifies the MPED/FPED intake of total meat   */
/*                       after legume allocation.                    */
/*                                                                   */
/*   "lseaplant"    * Specifies the MPED/FPED intake of fish and     */
/*                       plant protein after legume allocation.      */
/*                                                                   */
/*   "monopoly"     * Specifies the grams of monofat plus poly fat   */
/*                                                                   */
/*   "sfat"         * Specifies the grams of saturated fat.          */
/*                                                                   */
/*   "sodi"         * Specifies the mg of sodium.                    */
/*                                                                   */                                                                 
/*   "G_NWHL"       * Specifies the MPED/FPED intake of refined      */
/*                       grain                                       */
/*                                                                   */
/*                                                                   */                                                                  
/*   "emptycal10"   * Specifies the total amount of SoFAAS calories. */
/*                                                                   */
/*   "outdat"       * Specifies the name of the resulting dataset.   */
/*                                                                   */                                                                  
/* Caution:  variable names "SODMAX", "SODMIN", "SODMED",            */
/*   "SOFAMIN", "SOFAMAX" are reserved for this macro.               */
/*                                                                   */
/*                                                                   */
/*********************************************************************/
;




%macro HEI2010 (indat=,kcal=,lv_total=,lbeangrn=,f_total=,wholefrt=,g_whl=,d_total=,
  lallmeat=,lseaplant=,monopoly=,sfat=,sodi=,G_NWHL=,EMPTYCAL10=,outdat=);

data &outdat (drop=FARMIN FARMAX SODMAX SODMIN RGMIN RGMAX SOFAMIN SOFAMAX);
  set &indat;

  IF &kcal > 0 then VEGDEN=&LV_TOTAL/(&kcal/1000);
  HEIX1_TOTALVEG=5*(VEGDEN/1.1);
  IF HEIX1_TOTALVEG > 5 THEN HEIX1_TOTALVEG=5;
  IF VEGDEN=0 THEN HEIX1_TOTALVEG=0;

  IF &kcal > 0 then GRBNDEN=&LBEANGRN/(&kcal/1000);
  HEIX2_GREEN_AND_BEAN=5*(GRBNDEN/0.2);
  IF HEIX2_GREEN_AND_BEAN > 5 THEN HEIX2_GREEN_AND_BEAN=5;
  IF GRBNDEN=0 THEN HEIX2_GREEN_AND_BEAN=0;

  IF &kcal > 0 then FRTDEN=&F_TOTAL/(&kcal/1000);
  HEIX3_TOTALFRUIT=5*(FRTDEN/0.8);
  IF HEIX3_TOTALFRUIT > 5 THEN HEIX3_TOTALFRUIT=5;
  IF FRTDEN=0 THEN HEIX3_TOTALFRUIT=0;	

  IF &kcal > 0 then WHFRDEN=&WHOLEFRT/(&kcal/1000);
  HEIX4_WHOLEFRUIT=5*(WHFRDEN/0.4); 
  IF HEIX4_WHOLEFRUIT > 5 THEN HEIX4_WHOLEFRUIT=5;
  IF WHFRDEN=0 THEN HEIX4_WHOLEFRUIT=0;	

  IF &kcal > 0 then WGRNDEN=&G_WHL/(&kcal/1000);
  HEIX5_WHOLEGRAIN=10*(WGRNDEN/1.5);
  IF HEIX5_WHOLEGRAIN > 10 THEN HEIX5_WHOLEGRAIN=10;
  IF WGRNDEN=0 THEN HEIX5_WHOLEGRAIN=0;

  IF &kcal > 0 then DAIRYDEN=&D_TOTAL/(&kcal/1000);
  HEIX6_TOTALDAIRY=10*(DAIRYDEN/1.3);
  IF HEIX6_TOTALDAIRY > 10 THEN HEIX6_TOTALDAIRY=10;
  IF DAIRYDEN=0 THEN HEIX6_TOTALDAIRY=0;

  IF &kcal > 0 then MEATDEN=&LALLMEAT/(&kcal/1000);
  HEIX7_TOTPROT=5*(MEATDEN/2.5);
  IF HEIX7_TOTPROT > 5 THEN HEIX7_TOTPROT=5;
  IF MEATDEN=0 THEN HEIX7_TOTPROT=0;

  IF &kcal > 0 then SEAPLDEN=&LSEAPLANT/(&kcal/1000);
  HEIX8_SEAPLANT_PROT=5*(SEAPLDEN/0.8);
  IF HEIX8_SEAPLANT_PROT > 5 THEN HEIX8_SEAPLANT_PROT=5;
  IF SEAPLDEN=0 THEN HEIX8_SEAPLANT_PROT=0;

  IF &sfat > 0 THEN FARATIO=&monopoly/&sfat;
  FARMIN=1.2;
  FARMAX=2.5;
  if &sfat=0 and &monopoly=0 then HEIX9_FATTYACID=0;
    else if &sfat=0 and &monopoly > 0 then HEIX9_FATTYACID=10;
    else if FARATIO >= FARMAX THEN HEIX9_FATTYACID=10;
    else if FARATIO <= FARMIN THEN HEIX9_FATTYACID=0;
    else HEIX9_FATTYACID=10* ( (FARATIO-FARMIN) / (FARMAX-FARMIN) );

  IF &kcal > 0 then SODDEN=&SODI/&kcal;
  SODMIN=1.1;
  SODMAX=2.0;
  IF SODDEN <= SODMIN THEN HEIX10_SODIUM=10;
    ELSE IF SODDEN >= SODMAX THEN HEIX10_SODIUM=0;
    ELSE HEIX10_SODIUM=10 - (10 * (SODDEN-SODMIN) / (SODMAX-SODMIN) );

  IF &kcal > 0 then RGDEN=&G_NWHL/(&kcal/1000);
  RGMIN=1.8;
  RGMAX=4.3;
  IF RGDEN <= RGMIN THEN HEIX11_REFINEDGRAIN=10;
    ELSE IF RGDEN >= RGMAX THEN HEIX11_REFINEDGRAIN=0;
    ELSE HEIX11_REFINEDGRAIN=10 - ( 10* (RGDEN-RGMIN) / (RGMAX-RGMIN) ); 
      
  IF &kcal > 0 then SOFA_PERC=100*(&EMPTYCAL10/&kcal); 
  SOFAMIN=19;
  SOFAMAX=50;
  IF SOFA_PERC >= SOFAMAX THEN HEIX12_SOFAAS=0;
    ELSE IF SOFA_PERC <= SOFAMIN THEN HEIX12_SOFAAS=20;
    ELSE HEIX12_SOFAAS= 20 - ( 20* (SOFA_PERC-SOFAMIN) / (SOFAMAX-SOFAMIN) );



IF &kcal=0 THEN DO;
  HEIX1_TOTALVEG=0; HEIX2_GREEN_AND_BEAN=0; HEIX3_TOTALFRUIT=0; HEIX4_WHOLEFRUIT=0; HEIX5_WHOLEGRAIN=0; HEIX6_TOTALDAIRY=0;
  HEIX7_TOTPROT=0;  HEIX8_SEAPLANT_PROT=0; HEIX9_FATTYACID=0; HEIX10_SODIUM=0; HEIX11_REFINEDGRAIN=0; HEIX12_SOFAAS=0;
  END;

/**Calculate HEI-2010 total score**/
/*total HEI-2010 score is the sum of 12 HEI component scores*/

HEI2010_TOTAL_SCORE = HEIX1_TOTALVEG + HEIX2_GREEN_AND_BEAN + HEIX3_TOTALFRUIT + HEIX4_WHOLEFRUIT + HEIX5_WHOLEGRAIN + HEIX6_TOTALDAIRY +
  HEIX7_TOTPROT + HEIX8_SEAPLANT_PROT + HEIX9_FATTYACID + HEIX10_SODIUM + HEIX11_REFINEDGRAIN + HEIX12_SOFAAS;


LABEL HEI2010_TOTAL_SCORE='TOTAL HEI-2010 SCORE'
      HEIX1_TOTALVEG='HEI-2010 COMPONENT 1 TOTAL VEGETABLES'
      HEIX2_GREEN_AND_BEAN='HEI-2010 COMPONENT 2 GREENS AND BEANS'
      HEIX3_TOTALFRUIT='HEI-2010 COMPONENT 3 TOTAL FRUIT'
      HEIX4_WHOLEFRUIT='HEI-2010 COMPONENT 4 WHOLE FRUIT'
      HEIX5_WHOLEGRAIN='HEI-2010 COMPONENT 5 WHOLE GRAINS'
      HEIX6_TOTALDAIRY='HEI-2010 COMPONENT 6 DAIRY'
      HEIX7_TOTPROT='HEI-2010 COMPONENT 7 TOTAL PROTEIN FOODS'
      HEIX8_SEAPLANT_PROT='HEI-2010 COMPONENT 8 SEAFOOD AND PLANT PROTEIN'
      HEIX9_FATTYACID='HEI-2010 COMPONENT 9 FATTY ACID RATIO'
      HEIX10_SODIUM='HEI-2010 COMPONENT 10 SODIUM'
      HEIX11_REFINEDGRAIN='HEI-2010 COMPONENT 11 REFINED GRAINS'
      HEIX12_SOFAAS='HEI-2010 COMPONENT 12 SOFAAS CALORIES'
      VEGDEN='DENSITY OF MPED/FPED TOTAL VEGETABLES PER 1000 KCAL'
      GRBNDEN='DENSITY OF MPED/FPED OF DARK GREEN VEG AND BEANS PER 1000 KCAL'
      FRTDEN='DENSITY OF MPED/FPED TOTAL FRUIT PER 1000 KCAL'
      WHFRDEN='DENSITY OF MPED/FPED WHOLE FRUIT PER 1000 KCAL'
      WGRNDEN='DENSITY OF MPED/FPED OF WHOLE GRAIN PER 1000 KCAL'
      DAIRYDEN='DENSITY OF MPED/FPED OF DAIRY PER 1000 KCAL'
      MEATDEN='DENSITY OF MPED/FPED TOTAL MEAT/PROTEIN PER 1000 KCAL'
      SEAPLDEN='DENSTIY OF MPED/FPED OF SEAFOOD AND PLANT PROTEIN PER 1000 KCAL'
      FARATIO='FATTY ACID RATIO'
      SODDEN='DENSITY OF SODIUM PER 1000 KCAL'
      RGDEN='DENSITY OF MPED/FPED OF REFINED GRAINS PER 1000 KCAL'
      SOFA_PERC='PERCENT OF CALORIES FROM ADDED SUGAR, SOLID FAT, AND ALCOHOL';

run;


%mend HEI2010;




/*  END OF THE HEI2010 MACRO                                       */
/*******************************************************************/



