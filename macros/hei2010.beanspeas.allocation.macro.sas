/*********************************************************************/
/*********************************************************************/
/*                                                                   */
/*          THE LEGUME ALLOCATION MACRO FOR HEI-2010                 */
/*          (hei2010.beanspeas.allocation.macro.sas)                 */                              
/*                                                                   */
/*********************************************************************/
/*                     VERSION 1.1         03/14/2013                */
/*                                                                   */
/*                                                                   */
/* This legume allocation macro is to be used in conjunction with    */
/* the HEI-2010 scoring macro.                                       */
/*								     			   */
/* The purpose is to allocate beans and peas (legumes) into either   */
/* Meat and Beans or Total Vegetables and Dark Green and Orange	   */
/* Vegetables and Legumes. Intake of beans and peas counts as        */
/* Total Proteins (and Seafood and Plant Proteins) until the 	   */
/* standard is met, then the rest count toward Total Vegetables      */
/* and Greens and Beans.                                             */
/*                                                                   */
/* The resulting dataset, which is named by the user, contains the   */
/* same variables as the supplied dataset, and creates five new      */
/* variables.  They include:					               */
/*                                                                   */				    
/*   LEGTYPE = a character variable that represents if the legumes   */
/*       are added to meat ('ALLMEAT'), to vegetables ('ALLVEG')     */
/*       or distributed to both ('MEAT/VEG').  This variable is for  */
/*       informational purposes and is not required by the HEI-2010  */
/*       scoring macro.                                              */
/*                                                                   */
/*   Four additional variables that contain the total intake         */
/*    of Total Proteins, Seafood and Plant Proteins, Total           */
/*    Vegetables and Greens and Beans AFTER the appropriate amount   */
/*    of beans and peas has been added.  The new variables are       */
/*    named by appending 'legume_added_' to the original name        */
/*    of the two protein variables and Total Vegetables. A new       */
/*    variable named legume_added_BEANGRN is also created.           */
/*    								               */
/* The code has been created to work with variables from the 	   */
/* MyPyramid Equivalents Database but can be adapted to use          */
/* variables from other sources (e.g., data on the food supply).     */
/*										         */
/* The syntax for calling the macro is:                              */
/*                                                                   */
/* %LEGALLOC                                                         */
/* (indat=, kcal=, allmeat=, v_total=, v_dol=, legumes=, outdat=)    */ 
/*                                                                   */
/*  where                                                            */
/*                                                                   */
/*   "indat"        * Specifies the dataset to be used.              */
/*                                                                   */
/*   "kcal"         * Specifies calorie intake.                      */
/*                                                                   */
/*   "allmeat"      * Specifies sum of intake of 		         */
/*                      M_MPF+M_EGG+M_NUTSD+M_SOY.                   */
/*                                                                   */
/*   "seaplant"     * Specifies sum of intake of 		         */
/*                      M_FISH_HI+M_FISH_LO+M_SOY+M_NUTSD.           */
/*                                                                   */
/*   "v_total"      * Specifies the intake of V_TOTAL.               */
/*                                                                   */
/*   "v_drkgr"      * Specifies the intake of V_DRKGR.               */
/*                                                                   */
/*   "legumes"      * Specifies the intake of LEGUMES.               */
/*                                                                   */
/*   "outdat"       * Specifies the name of the resulting dataset.   */
/*                                                                   */
/*                                                                   */
/* Caution:  variable names "LEGTYPE", "MBMAX", "MEATLEG",           */
/*   "NEEDMEAT", "EXTRMEAT", "EXTRLEG" are reserved for this macro.  */
/*                                                                   */
/*                                                                   */
/*********************************************************************/
;


%macro LEG2010A (indat=,kcal=,allmeat=,seaplant=,v_total=,v_drkgr=,legumes=,outdat=);

data &outdat (drop=mbmax meatleg needmeat extrmeat extrleg);
  set &indat;

/**Allocate legumes to either meat or veg**/
/*Standard is 2.5 oz equiv/1000 kcal*/

MBMAX=2.5*(&KCAL/1000);	/*Create the Protein / Meat standard*/

 /*Legumes intake calculation*/
 /*Legumes intake counts as Protein / Meat until the standard is met, then the rest count as Vegetables*/

/*(a) If total meat intake is less than the Protein / Meat recommendation then 
  all Legumes go to Protein / Meat */
IF &ALLMEAT < MBMAX THEN DO;
   MEATLEG=&LEGUMES*4;	/*Convert cup equivalents of Legumes to oz equivalents */
   NEEDMEAT=MBMAX-&ALLMEAT;

   IF MEATLEG <= NEEDMEAT THEN DO;
      LEGTYPE='ALLMEAT ';
      legume_added_&ALLMEAT=&ALLMEAT+MEATLEG;
      legume_added_&SEAPLANT=&SEAPLANT+MEATLEG;
      legume_added_&V_TOTAL=&V_TOTAL;
      legume_added_BEANGRN=&V_DRKGR;
   END; 

/*(b) Some Legumes go to Protein / Meat and the rest goes to Vegetables*/
   ELSE IF MEATLEG > NEEDMEAT THEN DO; 
      LEGTYPE='MEAT/VEG';
      EXTRMEAT=MEATLEG-NEEDMEAT;
      EXTRLEG=EXTRMEAT/4; 	/*Convert oz equivalents of Meat and Beans from Legumes back to cup equivalents*/
      legume_added_&ALLMEAT=&ALLMEAT+NEEDMEAT;
      legume_added_&SEAPLANT=&SEAPLANT+NEEDMEAT;
      legume_added_&V_TOTAL=&V_TOTAL+EXTRLEG;
      legume_added_BEANGRN=&V_DRKGR+EXTRLEG;
     END;
END; 

/*(c) If total meat intake exceeds the Protein / Meat standard, then all Legumes count as Vegetables*/
ELSE IF &ALLMEAT >= MBMAX THEN DO;  
    LEGTYPE='ALLVEG';
    legume_added_&ALLMEAT=&ALLMEAT;
    legume_added_&SEAPLANT=&SEAPLANT;
    legume_added_&V_TOTAL=&V_TOTAL+&LEGUMES;
    legume_added_BEANGRN=&V_DRKGR+&LEGUMES;
END;

run;

%mend LEG2010A;

 
/*  END OF THE LEG2010A MACRO                                       */
/*******************************************************************/


