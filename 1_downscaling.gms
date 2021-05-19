* PART 1: READ INPUT AND PRECOMPUTE PARAMETERS
$setglobal project SSPxRCP
$setglobal lab     16042021

* Limpopo counter
$if not set nsim $set nsim 0
$set nsim %nsim%


Option limrow=0;
Option limcol=0;
Option profile=1;
FILE Results_SCEN;
FILE Results_Reg;

* Define sets
$Include .\source\decl_sets.gms
$include .\source\set_simU_reg_map_updated.gms

* Load GLOBIOM output
SET
REGION
COUNTRY
REGION_MAP
MacroScen
BioenScen
IEA_SCEN
ScenYear
;

PARAMETER
LUC_COMPARE_SCEN0(*,*,*,*,*,*,*)
Price_Compare2(*,*,*,*,*,*)
LANDCOVER_COMPARE_SCEN(*,*,*,*,*,*)
;

$GDXIN .\input\output_landcover_%project%_%lab%
$LOAD LANDCOVER_COMPARE_SCEN = LANDCOVER_COMPARE_SCEN
$LOAD LUC_COMPARE_SCEN0 = LUC_COMPARE_SCEN0
$LOAD Price_Compare2 = Price_Compare2
$LOAD REGION
$LOAD COUNTRY
$LOAD REGION_MAP
$LOAD MacroScen
$LOAD BioenScen
$LOAD IEA_SCEN
$LOAD ScenYear
$GDXIN

* Load data for downscaling
$offlisting

TABLE Transportation(SimUID,ATTRIBUTE1) Distance in minutes
$ondelim
$include .\source\acc_mean_travel_minutes_simu.csv
$offdelim

$include .\source\data_GrasYield_X.gms
$onlisting

PARAMETER
AREA(*,*,*,*)
SRP_Suit(*,*,*);
$GDXIN .\source\X_4Tatiana.gdx
$LOAD AREA
$LOAD SRP_Suit
$GDXIN

* Base year 2000 land use GEOBENE data rescaled to match GLOBIOMsol numbers at regional level
PARAMETER LUC_Fin(*,*);
$GDXIN .\source\LUC_Fin_Write_SSP2_msg07.gdx
$LOAD LUC_Fin
$GDXIN

PARAMETER Yield_Simu(*,*,*,*);
$GDXIN .\source\yields.gdx
$LOAD Yield_Simu
$GDXIN

PARAMETER MngForest_Param(*,*,*);
$GDXIN .\source\Forestparameters.gdx
$LOAD MngForest_Param
$GDXIN

* Scenario mapping for condor run
SET
ScenLOOP(AllScenLOOP)
/
%nsim%
/

SET MAP_ScenLOOP_ScenDims(AllScenLOOP,MacroScen,BioenScen,IEA_SCEN,REGION);
SET MAP_ScenLOOP_ScenDims2(MacroScen,BioenScen,IEA_SCEN,REGION);

MAP_ScenLOOP_ScenDims2(MacroScen,BioenScen,IEA_SCEN,REGION)
 $LANDCOVER_COMPARE_SCEN(REGION,'TotLnd',MacroScen,BioenScen,IEA_SCEN,'2000')=YES;

SCALAR ScenNumber /0/;

LOOP((MAP_ScenLOOP_ScenDims2(MacroScen,BioenScen,IEA_SCEN,REGION)),
MAP_ScenLOOP_ScenDims(AllScenLOOP,MacroScen,BioenScen,IEA_SCEN,REGION)
 $(AllScenLOOP.val eq ScenNumber) = YES;

ScenNumber = ScenNumber+1;
);
*$exit

* PART 2: DOWNSCALING
SCALAR delta /0.0001/ ;
SCALAR weight /10/ ;
SET rSimUID(SimUID);
ALIAS(rSimUID,i);

PARAMETER
SH_Opt(REGION,SimUID,LC_TYPES_EPIC,LC_TYPES_EPIC,BioenScen,ScenYear)
Delta_Opt(REGION,SimUID,LC_TYPES_EPIC,LC_TYPES_EPIC,BioenScen,ScenYear)
Sum_LandUse_Aux(REGION,LC_TYPES_EPIC,ScenYear)
* Used for calculating priors
Aux_Grass_NatLand
Aux_Grass_PltFor
Aux_CrpLnd_PltFor
AuxForest_Grass
AuxForest_CrpLnd
Aux_Nat_Land
Sum_AreaWeighted_Product_Tot
* Planted forest is not available in 2000 and has to be initialised
PltFor_Tot(REGION,BioenScen,ScenYear)
ShPltForInit(REGION,SimUID,BioenScen,ScenYear)
* Land use change at the beginning of the year
Delta_Init(REGION,LC_TYPES_EPIC,LC_TYPES_EPIC,ScenYear)
* Land use change at the end of the year
Delta_fin(REGION,LC_TYPES_EPIC,LC_TYPES_EPIC,ScenYear)
* Parameters for defining priors
Trans_Cost_Tot(REGION)
Sh_Trans_Cost(SimUID)
Inv_Trans_Cost(SimUID)
Inv_Trans_Cost_Tot(REGION)
Sh_Inv_Trans_Cost(SimUID)
PRODUCT_CROP_UNIT_INPUT(SimUID,CROP,INPUT_LEVEL)
Yield_CROP_UNIT_INPUT(SimUID,CROP,INPUT_LEVEL)
Area_CROP_UNIT_INPUT(SimUID,CROP,INPUT_LEVEL)
Sum_Crop_Area(SimUID)
Product_Tot(SimUID)
Sum_Product(REGION)
* Parameters for planted forest
Sh_PltFor(SimUID)
PltForSimUnit(SimUID)
* Parameters for calculating priors
Grass_Yield_SU(SimUID)
SimUnit_Yield(SimUID,REGION,CROP,INPUT_LEVEL)
Sum_Grass_Yield(REGION)
CROP_Area(SimUID,CROP,INPUT_LEVEL)
AreaWeighted_Product_Tot(SimUID)
Inv_Prod_SU(SimUID)
Sum_Inv_Prod(REGION)
Sh_Inv_Prod(SimUID)
Inv_Grass_Prod(SimUID)
Sum_Inv_Grass_Prod(REGION)
Sh_Inv_Grass_Prod(SimUID)
Sum_Inv_Prod_NatLand(REGION)
Sh_Inv_Prod_NatLand(SimUID)
PltForSum_NatLand
Sum_Product_NatLand
Sh_Crop_NatLand(SimUID)
Sum_Grass_NatLand
Aux(SimUID)
Aux_Trans(SimUID)
MIN_Trans
* prior shares
SH1(LC_TYPES_EPIC,LC_TYPES_EPIC,SimUID)
* Consistency checks, Auxiliary variables
SH1_Check_sum(LC_TYPES_EPIC,LC_TYPES_EPIC,ScenYear)
X_Check_sum(LC_TYPES_EPIC,LC_TYPES_EPIC,ScenYear)
Sh(REGION,SimUID,LC_TYPES_EPIC,LC_TYPES_EPIC,BioenScen,ScenYear)
* for land accounting
DDelta(LC_TYPES_EPIC,LC_TYPES_EPIC)
SimUnitArea(SimUID)
Land_Cover_SU(SimUID,LC_TYPES_EPIC)
Land_Cover_SU_tt(SimUID,LC_TYPES_EPIC,ScenYear)
Land_Cover_SU_Region(REGION,SimUID,LC_TYPES_EPIC,ScenYear)
sum_Land_Cover_tt_after(REGION,LC_TYPES_EPIC,ScenYear)
Delta_LAND_Region(REGION,LC_TYPES_EPIC,ScenYear)
Delta_LAND
* Consistency checks, Auxiliary variables, can be deleted
SH_EQ_Check(LC_TYPES_EPIC1,LC_TYPES_EPIC2,ScenYear)
Aus_Reg(REGION,LC_TYPES_EPIC,ScenYear)
Aus_SU(REGION,LC_TYPES_EPIC,ScenYear)
Land_Pos(REGION,LC_TYPES_EPIC,SimUID,ScenYear)
CHECK_X_SH(LC_TYPES_EPIC,LC_TYPES_EPIC,SimUID,ScenYear)
CHECK_X_SH_ABS(SOL,LC_TYPES_EPIC,LC_TYPES_EPIC,SimUID,ScenYear)
;

*option DOWNSCALE_07_01_12.holdfixed = 1 ;
Option solvelink = 0 ;

* Downscaling model
VARIABLES
Z_VAR                                                    entropy
INTERM1_VAR(LC_TYPES_EPIC,LC_TYPES_EPIC,SimUID)
;

POSITIVE VARIABLE
X_VAR(LC_TYPES_EPIC,LC_TYPES_EPIC,SimUID)
Dummy_VAR(SimUID)
aa_VAR(LC_TYPES_EPIC,LC_TYPES_EPIC)
LndP_VAR(LC_TYPES_EPIC,SimUID)
;

EQUATIONS
ENTROPY_EQU                                              Cross-entropy equation
SH_EQU(LC_TYPES_EPIC,LC_TYPES_EPIC)
Sum_LAND_SU_EQU(SimUID)                                  Total land in sim unit equals to SimUarea
Land_Positive_SU_EQU(LC_TYPES_EPIC,SimUID)
Delta_Land_00_EQU(LC_TYPES_EPIC)
;


ENTROPY_EQU..
  Z_VAR
  =E=
*Cross-enthropy part
    SUM((rSimUID,LC_TYPES_EPIC1,LC_TYPES_EPIC2)
       $((NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND
         (SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)) AND
         (DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2) >0)),
    INTERM1_VAR(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID))*0.0000001

* Minimize any mismatches (Dummy_VAR) in land equation Sum_LAND_SU_EQU(rSimUID).. (land of all types E= SimUnitArea(rSimUID)
* weight is used to penalize the mismatch
  + SUM((rSimUID)$(Land_Cover_SU(rSimUID,'SimUarea')),
    weight*(Dummy_VAR(rSimUID)))

* Minimize any mismatches (aa_VAR) in Shares accountng SH_EQU(LC_TYPES_EPIC1,LC_TYPES_EPIC2) , weight is used to penalize the mismatch
  + weight*(SUM((LC_TYPES_EPIC1,LC_TYPES_EPIC2)
         $((NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND
           (DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2) >0)),
    aa_VAR(LC_TYPES_EPIC1,LC_TYPES_EPIC2)))

* Minimize any mismatches (LndP_VAR) in land use change Land_Positive_SU_EQU .. , weight is used to penalize the mismatch
  + weight*SUM((LC_TYPES_EPIC,rSimUID),
    LndP_VAR(LC_TYPES_EPIC,rSimUID));

* Consistency check for X_VAR, sum X_VARs has to be 1, otherwise aa_VAR is non zero
SH_EQU(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
          $((NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND
            (DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2)>0))..

    SUM((rSimUID)
       $(SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)),
    X_VAR(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID))
  + aa_VAR(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
    =E= 1;


Sum_LAND_SU_EQU(rSimUID)..

     SUM((LC_TYPES_EPIC1, LC_TYPES_EPIC2)
      $((NOT sameas (LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND
        (SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID) )),
    X_VAR(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)
  * DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
         $(SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)))
  - SUM((LC_TYPES_EPIC2,LC_TYPES_EPIC1)
         $((NOT sameas (LC_TYPES_EPIC2, LC_TYPES_EPIC1)) AND
           (SH1(LC_TYPES_EPIC2,LC_TYPES_EPIC1,rSimUID) )),
    X_VAR(LC_TYPES_EPIC2,LC_TYPES_EPIC1,rSimUID)
  * DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC1)
         $(SH1(LC_TYPES_EPIC2,LC_TYPES_EPIC1,rSimUID)))
  + SUM(LC_TYPES_EPIC1
         $(NOT sameas(LC_TYPES_EPIC1,'SimUarea')),
    Land_Cover_SU(rSimUID,LC_TYPES_EPIC1))
  + Dummy_VAR(rSimUID)
    =E= SimUnitArea(rSimUID);

* Consistency check in land use changes using X_VAR ,
* X_VAR(LC_TYPES_EPIC2,LC_TYPES_EPIC,rSimUID) at the level of rSimUID is applied to total DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC)

Land_Positive_SU_EQU(LC_TYPES_EPIC,rSimUID)
         $(NOT sameas(LC_TYPES_EPIC,'SimUarea'))..

    Land_Cover_SU(rSimUID,LC_TYPES_EPIC)

  + SUM(LC_TYPES_EPIC2
          $(NOT sameas(LC_TYPES_EPIC2,LC_TYPES_EPIC)),
    X_VAR(LC_TYPES_EPIC2,LC_TYPES_EPIC,rSimUID)*DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC))

  - SUM(LC_TYPES_EPIC2
          $(NOT sameas(LC_TYPES_EPIC,LC_TYPES_EPIC2)),
    X_VAR(LC_TYPES_EPIC,LC_TYPES_EPIC2,rSimUID)
  * DDelta(LC_TYPES_EPIC,LC_TYPES_EPIC2))

  + LndP_VAR(LC_TYPES_EPIC,rSimUID)
    =G= 0;


Delta_Land_00_EQU(LC_TYPES_EPIC1)
         $(NOT sameas(LC_TYPES_EPIC1,'SimUarea'))..

    SUM((rSimUID, LC_TYPES_EPIC2)
         $(NOT sameas (LC_TYPES_EPIC2,LC_TYPES_EPIC1) AND
          (DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC1) > 0)),
    X_VAR(LC_TYPES_EPIC2,LC_TYPES_EPIC1,rSimUID)
  * DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC1)
         $(SH1(LC_TYPES_EPIC2,LC_TYPES_EPIC1,rSimUID))

  - X_VAR(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)
  * DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
         $(SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)) )
    =E=

    SUM(LC_TYPES_EPIC2
      $((NOT sameas (LC_TYPES_EPIC2,LC_TYPES_EPIC1)) AND
        (DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC1) > 0)),
    DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC1)
  - DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2));

MODEL ENTROPYMAX/
      ENTROPY_EQU
      SH_EQU
      Sum_LAND_SU_EQU
      Land_Positive_SU_EQU
      Delta_Land_00_EQU/;


* Initialize priors
LOOP(MAP_ScenLOOP_ScenDims(ScenLOOP,MacroScen,BioenScen,IEA_SCEN,REGION),

 rSimUID(SimUID)
  $  SUM(COUNTRY $ REGION_MAP(REGION,COUNTRY),
    SimUID_reg_MAP(SimUID,COUNTRY))
  = YES ;

LOOP(ScenYear,

* Initialization of land use change to be equal to LUC_COMPARE_SCEN0(REGION,LUC_Set1,LUC_Set2,MacroScen,BioenScen,IEA_SCEN,ScenYear)
DDelta(LC_TYPES_EPIC,LC_TYPES_EPIC) = 0;
DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
 = SUM((LC_MAP_EPIC_LUCSET(LC_TYPES_EPIC1,LUC_Set1),LC_MAP_EPIC_LUCSET2(LC_TYPES_EPIC2,LUC_Set2)),
   LUC_COMPARE_SCEN0(REGION,LUC_Set1,LUC_Set2,MacroScen,BioenScen,IEA_SCEN,ScenYear));

* if for whatever reason DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2) is negative, set it to 0
DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
 $(DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2) < 0.00001)
 = 0 ;

* Initialize variable Delta_Init to be equal to DDelta, before any changes are done to DDelta
Delta_Init(REGION, LC_TYPES_EPIC1,LC_TYPES_EPIC2,ScenYear)
 = DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2);

IF((ord(ScenYear) eq 1),
* in the first time period, land cover in simulation unit (Land_Cover_SU(rSimUID,LC_TYPES_EPIC)) set to LUC_Fin(rSimUID,LC_TYPES_EPIC)
Land_Cover_SU(rSimUID,LC_TYPES_EPIC) = LUC_Fin(rSimUID,LC_TYPES_EPIC) ;
Land_Cover_SU(rSimUID,LC_TYPES_EPIC) $(Land_Cover_SU(rSimUID,LC_TYPES_EPIC) < 0) = 0 ;

* Initialize Planted forst as some share of Land_Cover_SU(rSimUID,'OthNatLnd'). In Initial year, land use data for planted forest is absent
PltFor_Tot(REGION,BioenScen,ScenYear) = LANDCOVER_COMPARE_SCEN(REGION,'PltForTot',MacroScen,BioenScen,IEA_SCEN,ScenYear) ;

Aux_Nat_Land = sum(rSimUID $(Land_Cover_SU(rSimUID,'OthNatLnd') > 0), Land_Cover_SU(rSimUID,'OthNatLnd')) ;
ShPltForInit(REGION,rSimUID,BioenScen,ScenYear) = Land_Cover_SU(rSimUID,'OthNatLnd') /Aux_Nat_Land ;

Land_Cover_SU(rSimUID,'PltFor') =  PltFor_Tot(REGION,BioenScen,ScenYear) * ShPltForInit(REGION,rSimUID,BioenScen,ScenYear) ;

* Subtract PlantedForest from OthNatLnd
Land_Cover_SU(rSimUID,'OthNatLnd') = Land_Cover_SU(rSimUID,'OthNatLnd') - Land_Cover_SU(rSimUID,'PltFor') ;

* keep track of land dynamics at the levelof SimUID, Land_Cover_SU will be reinitialized
Land_Cover_SU_tt(rSimUID,LC_TYPES_EPIC,ScenYear) = Land_Cover_SU(rSimUID,LC_TYPES_EPIC) ;

* "Yield" or "productivity" of planted forest
* Initialize the variable, not to be equal to 0
PltForSimUnit(rSimUID)  = 0.01 ;
PltForSimUnit(rSimUID)
 = sum(SimUID_reg_MAP(rSimUID,COUNTRY),SRP_suit(rSimUID,COUNTRY,'SRP_NPP')) ;
* If the above PltForSimUnit(rSimUID) equal to 0, set is to small positive
PltForSimUnit(rSimUID) $(PltForSimUnit(rSimUID) = 0)  = 0.01 ;

* Calculate Total land in simulation unit as a sum of different land types
SimUnitArea(rSimUID) = Land_Cover_SU(rSimUID,'CrpLnd')+ Land_Cover_SU(rSimUID,'Forest')
 + Land_Cover_SU(rSimUID,'Grass') + Land_Cover_SU(rSimUID,'OthNatLnd') + Land_Cover_SU(rSimUID,'PltFor') ;

ELSE
* From the second timeperiod, initialize the variable
Land_Cover_SU(rSimUID,LC_TYPES_EPIC) = 0 ;
* Set Land_Cover_SU(rSimUID,LC_TYPES_EPIC) equal to land cover fromthe previous time period
Land_Cover_SU(rSimUID,LC_TYPES_EPIC) = Land_Cover_SU_tt(rSimUID,LC_TYPES_EPIC,ScenYear-1) ;

* Keep track of dynamics, Land_Cover_SU at the beginning of time ScenYear equals Land_Cover_SU at the end of t-1
Land_Cover_SU_tt(rSimUID,LC_TYPES_EPIC,ScenYear)
 = Land_Cover_SU_tt(rSimUID,LC_TYPES_EPIC,ScenYear-1) ;
);

* Transportation costs and other related variables, to be used for calculation of prior shares
Aux_Trans(rSimUID) $(SimUnitArea(rSimUID) >0) = Transportation(rSimUID,'MEAN1') ;
MIN_Trans = smin (rSimUID , Aux_Trans(rSimUID) ) ;
Trans_Cost_Tot(REGION) = sum(rSimUID, Aux_Trans(rSimUID) ) ;
Sh_Trans_Cost(rSimUID) $(Trans_Cost_Tot(REGION)>0) = Transportation(rSimUID,'MEAN1')/(Trans_Cost_Tot(REGION)) ;

* Here we take transportation distance as a proxy for transportation cost and
* "discount" the value of location by its distance to "large" city or a market place
* 0.5 is 50% "influence" of transportation distance
Inv_Trans_Cost(rSimUID) $(Transportation(rSimUID,'MEAN1')>0) = 1/(0.4*Transportation(rSimUID,'MEAN1')) ;
Inv_Trans_Cost_Tot(REGION) = 0;
Inv_Trans_Cost_Tot(REGION) = sum(rSimUID, Inv_Trans_Cost(rSimUID)) ;
Sh_Inv_Trans_Cost(rSimUID) $(Inv_Trans_Cost_Tot(REGION)>0) = Inv_Trans_Cost(rSimUID) / (Inv_Trans_Cost_Tot(REGION)) ;

* Value of crop production by crop and input level
Yield_CROP_UNIT_INPUT(rSimUID,CROP,INPUT_LEVEL)
 = sum(SimUID_reg_MAP(rSimUID,COUNTRY), Yield_Simu(rSimUID,COUNTRY,CROP,INPUT_LEVEL)) ;

Area_CROP_UNIT_INPUT(rSimUID,CROP,INPUT_LEVEL)
 = sum(SimUID_reg_MAP(rSimUID,COUNTRY), Area(rSimUID,COUNTRY,CROP,INPUT_LEVEL)) ;

Sum_Crop_Area(rSimUID)
 = sum ((CROP,INPUT_LEVEL), Area_CROP_UNIT_INPUT(rSimUID,CROP,INPUT_LEVEL) ) ;

PRODUCT_CROP_UNIT_INPUT(rSimUID,CROP,INPUT_LEVEL)
 = Yield_CROP_UNIT_INPUT(rSimUID,CROP,INPUT_LEVEL) * Area_CROP_UNIT_INPUT(rSimUID,CROP,INPUT_LEVEL) ;

* Production total
Product_Tot(rSimUID)
 = sum((CROP,INPUT_LEVEL),Price_Compare2(Crop,REGION,MacroScen,BioenScen,IEA_SCEN,ScenYear)* PRODUCT_CROP_UNIT_INPUT(rSimUID,CROP,INPUT_LEVEL)) ;

AreaWeighted_Product_Tot(rSimUID) $(Sum_Crop_Area(rSimUID) > 0 )
 = Product_Tot(rSimUID) / Sum_Crop_Area(rSimUID) ;

* Invers production value
Inv_Prod_SU(rSimUID) $(Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID)>0) =1/(Inv_Trans_Cost(rSimUID)*1.5*AreaWeighted_Product_Tot(rSimUID)) ;

Sum_Inv_Prod(REGION)  = 0 ;
Sum_Inv_Prod(REGION)
 =  sum(rSimUID $( Land_Cover_SU(rSimUID,'CrpLnd') > 0) , Inv_Prod_SU(rSimUID)) ;

Sh_Inv_Prod(rSimUID) = 0 ;
Sh_Inv_Prod(rSimUID) $((Sum_Inv_Prod(REGION)>0) AND ( Land_Cover_SU(rSimUID,'CrpLnd') > 0)) = Inv_Prod_SU(rSimUID) / (Sum_Inv_Prod(REGION)) ;

* Defining Prior shares of land use conversion
* Crop to Grass land
SH1('CrpLnd','Grass',rSimUID)  = 0 ;
SH1('CrpLnd','Grass',rSimUID)  = Sh_Inv_Prod(rSimUID) ;

Sum_Product(REGION) = 0 ;
Sum_Product(REGION)
 = sum (rSimUID $( Land_Cover_SU(rSimUID,'CrpLnd') > 0), Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID)) ;

* Grass yields
* Since SimUs are not of the same size, we take productivity per hectar as a basis for prior
Grass_Yield_SU(rSimUID)
  = sum(SimUID_reg_MAP(rSimUID,COUNTRY),GRASYIELD(rSimUID,COUNTRY)) ;
Grass_Yield_SU(rSimUID) $(Grass_Yield_SU(rSimUID) = 0 ) = 0.001 ;
Sum_Grass_Yield(REGION)=sum(rSimUID,Grass_Yield_SU(rSimUID)) ;

* 'CrpLnd' to 'OthNatLnd'
Sh_Inv_Prod_NatLand(rSimUID) = 0 ;
Sum_Inv_Prod_NatLand(REGION)
 =  sum(rSimUID $(Land_Cover_SU(rSimUID,'CrpLnd') > 0), Inv_Prod_SU(rSimUID)) ;

Sh_Inv_Prod_NatLand(rSimUID) $((Sum_Inv_Prod_NatLand(REGION)>0) AND (Land_Cover_SU(rSimUID,'CrpLnd') > 0)) = Inv_Prod_SU(rSimUID) / (Sum_Inv_Prod_NatLand(REGION)) ;

SH1('CrpLnd','OthNatLnd',rSimUID) = 0 ;
SH1('CrpLnd','OthNatLnd',rSimUID)= Sh_Inv_Prod_NatLand(rSimUID) ;

* 'CrpLnd' to 'PltFor'
Aux_CrpLnd_PltFor =
 sum((rSimUID) $((AreaWeighted_Product_Tot(rSimUID) >0) AND (Inv_Trans_Cost(rSimUID) >0) AND (Land_Cover_SU(rSimUID,'CrpLnd') > 0)) , PltForSimUnit(rSimUID)*(1/(Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID)))) ;
SH1('CrpLnd','PltFor',rSimUID) = 0 ;
SH1('CrpLnd','PltFor',rSimUID) $((Aux_CrpLnd_PltFor > 0) AND (AreaWeighted_Product_Tot(rSimUID) >0) AND (Inv_Trans_Cost(rSimUID) >0) AND (Land_Cover_SU(rSimUID,'CrpLnd') > 0))
  = PltForSimUnit(rSimUID)*(1/(Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID))) / Aux_CrpLnd_PltFor ;

* 'Grass' to 'CrpLnd'
Sum_AreaWeighted_Product_Tot = sum(rSimUID $(Land_Cover_SU(rSimUID,'Grass') > 0), Land_Cover_SU(rSimUID,'Grass')) ;
SH1('Grass','CrpLnd',rSimUID) = 0 ;
SH1('Grass','CrpLnd',rSimUID)  $((Sum_AreaWeighted_Product_Tot > 0) AND (Land_Cover_SU(rSimUID,'Grass') >0))  = Land_Cover_SU(rSimUID,'Grass') / Sum_AreaWeighted_Product_Tot ;

* 'Grass' to 'PltFor'
Inv_Grass_Prod(rSimUID) $(Grass_Yield_SU(rSimUID)>0) = 1/ (Grass_Yield_SU(rSimUID)) ;
Sum_Inv_Grass_Prod(REGION)= sum (rSimUID $(Land_Cover_SU(rSimUID,'Grass')>0), Inv_Grass_Prod(rSimUID)) ;
Sh_Inv_Grass_Prod(rSimUID) $((Sum_Inv_Grass_Prod(REGION)>0) AND (Land_Cover_SU(rSimUID,'Grass')>0)) = Inv_Grass_Prod(rSimUID) / (Sum_Inv_Grass_Prod(REGION)) ;

Aux_Grass_PltFor
 = sum((rSimUID) $((Inv_Grass_Prod(rSimUID)>0) AND (PltForSimUnit(rSimUID) >0) AND (Land_Cover_SU(rSimUID,'Grass') > 0)) , Land_Cover_SU(rSimUID,'Grass') ) ;

SH1('Grass','PltFor',rSimUID) = 0 ;
SH1('Grass','PltFor',rSimUID) $((Aux_Grass_PltFor > 0) AND (Inv_Grass_Prod(rSimUID)>0) AND (PltForSimUnit(rSimUID) >0) AND (Land_Cover_SU(rSimUID,'Grass') > 0)) = Land_Cover_SU(rSimUID,'Grass') / Aux_Grass_PltFor ;

* 'Grass' to 'OthNatLnd'
Inv_Grass_Prod(rSimUID) $(Grass_Yield_SU(rSimUID)>0) = 1/ (Grass_Yield_SU(rSimUID)) ;
Sum_Inv_Grass_Prod(REGION)= sum (rSimUID $(Land_Cover_SU(rSimUID,'Grass')>0), Inv_Grass_Prod(rSimUID)) ;
Sh_Inv_Grass_Prod(rSimUID) $((Sum_Inv_Grass_Prod(REGION)>0) AND (Land_Cover_SU(rSimUID,'Grass')>0)) = Inv_Grass_Prod(rSimUID) / (Sum_Inv_Grass_Prod(REGION)) ;

Aux_Grass_NatLand = 0 ;
Aux_Grass_NatLand
 = sum((rSimUID) $(Land_Cover_SU(rSimUID,'Grass') > 0) , Land_Cover_SU(rSimUID,'Grass') ) ;

SH1('Grass','OthNatLnd',rSimUID) = 0 ;
SH1('Grass','OthNatLnd',rSimUID) = Land_Cover_SU(rSimUID,'Grass')/Aux_Grass_NatLand  ;

* 'PltFor' to 'OthNatLnd'
SH1('PltFor','OthNatLnd',rSimUID) = 0 ;
SH1('PltFor','OthNatLnd',rSimUID) = Sh_Inv_Trans_Cost(rSimUID)  ;

* 'Forest' to 'CrpLnd'
AuxForest_CrpLnd = 0;
AuxForest_CrpLnd
 = sum( (rSimUID) $((Land_Cover_SU(rSimUID,'Forest') > 0 ) AND (MngForest_Param(rSimUID,'CurN','HarvWood') >0) AND (MngForest_Param(rSimUID,'CurN','HarvCost') >0)) , MngForest_Param(rSimUID,'CurN','HarvCost') * (1/(Land_Cover_SU(rSimUID,'Forest')*MngForest_Param(rSimUID,'CurN','HarvWood')))*Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID)) ;

SH1('Forest','CrpLnd',rSimUID) = 0;
SH1('Forest','CrpLnd',rSimUID) $((Land_Cover_SU(rSimUID,'Forest') > 0 ) AND (MngForest_Param(rSimUID,'CurN','HarvWood') >0) AND (MngForest_Param(rSimUID,'CurN','HarvCost') >0) AND (AuxForest_CrpLnd >0)) = MngForest_Param(rSimUID,'CurN','HarvCost')* (1/(Land_Cover_SU(rSimUID,'Forest')*MngForest_Param(rSimUID,'CurN','HarvWood')))*Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID) / AuxForest_CrpLnd ;

* 'Forest' to 'Grass'
AuxForest_Grass = 0;
AuxForest_Grass
 = sum((rSimUID) $(Land_Cover_SU(rSimUID,'Forest') > 0 ) , Land_Cover_SU(rSimUID,'Forest')) ;

SH1('Forest','Grass',rSimUID) = 0 ;
SH1('Forest','Grass',rSimUID) $(Land_Cover_SU(rSimUID,'Forest') > 0 ) = Land_Cover_SU(rSimUID,'Forest') / AuxForest_Grass ;

* 'OthNatLnd' to 'CrpLnd'
Sum_Product_NatLand = 0 ;
Sum_Product_NatLand
 = sum (rSimUID $(Land_Cover_SU(rSimUID,'OthNatLnd') > 0 ), Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID)) ;

Aux(rSimUID) $(Land_Cover_SU(rSimUID,'OthNatLnd') > 0 ) =  Inv_Trans_Cost(rSimUID)*AreaWeighted_Product_Tot(rSimUID) ;

SH1('OthNatLnd','CrpLnd',rSimUID) = 0 ;
SH1('OthNatLnd','CrpLnd',rSimUID) $((Sum_Product_NatLand > 0) AND (Land_Cover_SU(rSimUID,'OthNatLnd') > 0 )) = Aux(rSimUID) /Sum_Product_NatLand ;

* 'OthNatLnd' to 'Grass'
Sum_Grass_NatLand =0 ;
Sum_Grass_NatLand = sum (rSimUID $(Land_Cover_SU(rSimUID,'OthNatLnd') > 0 ), Land_Cover_SU(rSimUID,'OthNatLnd')) ;

SH1('OthNatLnd','Grass',rSimUID) = 0 ;
SH1('OthNatLnd','Grass',rSimUID) $((Sum_Grass_NatLand > 0) AND (Land_Cover_SU(rSimUID,'OthNatLnd') > 0 )) = Land_Cover_SU(rSimUID,'OthNatLnd') / Sum_Grass_NatLand;

* 'OthNatLnd' to 'PltFor'
PltForSum_NatLand = 0 ;
PltForSum_NatLand = sum(rSimUID $(Land_Cover_SU(rSimUID,'OthNatLnd') > 0.0), PltForSimUnit(rSimUID)) ;

SH1('OthNatLnd','PltFor',rSimUID) = 0 ;
SH1('OthNatLnd','PltFor',rSimUID) $((PltForSum_NatLand >0) AND (Land_Cover_SU(rSimUID,'OthNatLnd') > 0.0))  = PltForSimUnit(rSimUID)/PltForSum_NatLand ;

* Initializing unknown shares
X_VAR.UP(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID) $((NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND (SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID))) = 1 ;
X_VAR.UP(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID) $((NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND (SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)= 0)) = 0 ;
X_VAR.LO(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID) $((NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND (SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID))) = 0 ;
X_VAR.L(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)  $((NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)) AND (SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID))) = SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID) ;

Sh(REGION,rSimUID, LC_TYPES_EPIC1,LC_TYPES_EPIC2,BioenScen,ScenYear)
 = SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID);

*option iterlim = 2000 ;
*option optcr   = 0.0001;

SOLVE ENTROPYMAX USING NLP MINIMIZING Z_VAR ;

Delta_fin(REGION,LC_TYPES_EPIC1,LC_TYPES_EPIC2,ScenYear)
 $((DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2) > 0) AND
   (NOT sameas(LC_TYPES_EPIC1,LC_TYPES_EPIC2)))
 = SUM(rSimUID
      $(X_VAR.l(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID) >0),
   X_VAR.l(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)
  *DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
         $(SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID))) ;

Delta_LAND(LC_TYPES_EPIC1,rSimUID,ScenYear)
 = - SUM(LC_TYPES_EPIC2,
     X_VAR.l(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)
    *DDelta(LC_TYPES_EPIC1,LC_TYPES_EPIC2)
         $(SH1(LC_TYPES_EPIC1,LC_TYPES_EPIC2,rSimUID)))
   + SUM(LC_TYPES_EPIC2,
     X_VAR.l(LC_TYPES_EPIC2,LC_TYPES_EPIC1,rSimUID)
    *DDelta(LC_TYPES_EPIC2,LC_TYPES_EPIC1)
         $(SH1(LC_TYPES_EPIC2,LC_TYPES_EPIC1,rSimUID)));

Delta_LAND_Region(REGION,LC_TYPES_EPIC1,ScenYear)
 = SUM(rSimUID
         $(Delta_LAND(LC_TYPES_EPIC1,rSimUID,ScenYear)>0),
   Delta_LAND(LC_TYPES_EPIC1,rSimUID,ScenYear));

Land_Cover_SU(rSimUID,LC_TYPES_EPIC1)
 = Land_Cover_SU(rSimUID,LC_TYPES_EPIC1)
 + Delta_LAND(LC_TYPES_EPIC1,rSimUID,ScenYear);

SimUnitArea(rSimUID)
 = SUM(LC_TYPES_EPIC1
     $(NOT sameas(LC_TYPES_EPIC1,'SimUarea')),
   Land_Cover_SU(rSimUID,LC_TYPES_EPIC1));

Land_Cover_SU(rSimUID,'SimUarea')
 = SimUnitArea(rSimUID);

Land_Cover_SU_tt(rSimUID,LC_TYPES_EPIC,ScenYear)
 = Land_Cover_SU(rSimUID,LC_TYPES_EPIC);

* Summary tables
sum_Land_Cover_tt_after(REGION,LC_TYPES_EPIC,ScenYear)
 = SUM(rSimUID, Land_Cover_SU(rSimUID,LC_TYPES_EPIC));

Land_Cover_SU_Region(REGION,rSimUID,LC_TYPES_EPIC,ScenYear)=0 ;
Land_Cover_SU_Region(REGION,rSimUID,LC_TYPES_EPIC,ScenYear)
 = Land_Cover_SU(rSimUID,LC_TYPES_EPIC);

* "Clean" variables for the next itteration/loop
Option kill= X_VAR;
Option kill= Z_VAR;
Option kill= ENTROPY_EQU ;
Option kill= SH1 ;
Option kill= SH_EQU;
Option kill= Sum_LAND_SU_EQU;
Option kill= Land_Cover_SU;
Option kill= Land_Positive_SU_EQU;
Option kill= Delta_Land_00_EQU;
Option kill= DDelta ;
Option kill= INTERM1_VAR ;

););

Option clear = rSimUID ;


* PART 3: MAPPING TO G4Mm REPORTING
PARAMETER
Land_Cover_SU_Region_SCEN(REGION,SimUID,LC_TYPES_EPIC,MacroScen,BioenScen,IEA_SCEN,ScenYear);

LOOP(MAP_ScenLOOP_ScenDims(ScenLOOP,MacroScen,BioenScen,IEA_SCEN,REGION),
Land_Cover_SU_Region(REGION,SimUID,LC_TYPES_EPIC,ScenYear) $(Land_Cover_SU_Region (REGION,SimUID,LC_TYPES_EPIC,ScenYear) < 0) = 0 ;
Land_Cover_SU_Region_SCEN(REGION,SimUID,LC_TYPES_EPIC,MacroScen,BioenScen,IEA_SCEN,ScenYear) = Land_Cover_SU_Region(REGION,SimUID,LC_TYPES_EPIC,ScenYear) ;
);

$include .\source\set_g4mIDsimUIDmap.gms

SET
Rg4m_05_id(g4m_05_id);
Rg4m_05_id(g4m_05_id) $ SUM(G4MID_SIMUID_MAP(g4m_05_id,SimUID),1) = YES;

PARAMETER
LandCover_G4MID_0(SimUID,MacroScen,BioenScen,IEA_SCEN,LandTypeFAO,ScenYear)
LandCover_G4MID_1(g4m_05_id,SimUID,MacroScen,IEA_SCEN,BioenScen,LandTypeFAO,ScenYear)
LandCover_G4MID(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,*,ScenYear);

LandCover_G4MID_0(SimUID,MacroScen,BioenScen,IEA_SCEN,LandTypeFAOg4m,ScenYear)
 = SUM((REGION,LandType_MAP(LC_TYPES_EPIC,LandTypeFAOg4m)),
      Land_Cover_SU_Region_SCEN(REGION,SimUID,LC_TYPES_EPIC,MacroScen,BioenScen,IEA_SCEN,ScenYear));

LOOP(G4MID_SIMUID_MAP(Rg4m_05_id,SimUID),
LandCover_G4MID_1(Rg4m_05_id,SimUID,MacroScen,IEA_SCEN,BioenScen,LandTypeFAOg4m,ScenYear)
 = LandCover_G4MID_0(SimUID,MacroScen,BioenScen,IEA_SCEN,LandTypeFAOg4m,ScenYear);
);

LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,LandTypeFAOg4m,ScenYear)
 = SUM(G4MID_SIMUID_MAP(Rg4m_05_id,SimUID),
        LandCover_G4MID_1(Rg4m_05_id,SimUID,MacroScen,IEA_SCEN,BioenScen,LandTypeFAOg4m,ScenYear));

LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear)
  $ LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"TotLnd",ScenYear)
 =  LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"Reserved",ScenYear)
  / LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"TotLnd",ScenYear);

LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear)
 $((LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear) ge 1) AND
    LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"TotLndnew",ScenYear))
 = LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"Reserved",ScenYear)
  /LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"TotLndnew",ScenYear);


execute_unload 'gdx\downscaled.gdx',
Delta_fin, Delta_Init, Delta_LAND_Region, sum_Land_Cover_tt_after, Land_Cover_SU_Region, Land_Cover_SU_Region_SCEN, Delta_LAND_Region, LandCover_G4MID
;
