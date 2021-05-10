$set project SSPxRCP
$set lab 5052021

$call gdxmerge gdx\output_%project%-*.gdx output=gdx\output_%project%_merged.gdx5052021

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
LANDCOVER_COMPARE_SCEN(*,*,*,*,*,*)
;

$GDXIN .\input\output_landcover_%project%_%lab%
$LOAD LANDCOVER_COMPARE_SCEN = LANDCOVER_COMPARE_SCEN
$LOAD REGION
$LOAD COUNTRY
$LOAD REGION_MAP
$LOAD MacroScen
$LOAD BioenScen
$LOAD IEA_SCEN
$LOAD ScenYear
$GDXIN

*SET AllScenLOOP2/output_%project%-0*output_%project%-20000/;

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


$include .\source\set_g4mIDsimUIDmap.gms

SET
Rg4m_05_id(g4m_05_id);
Rg4m_05_id(g4m_05_id) $ SUM(G4MID_SIMUID_MAP(g4m_05_id,SimUID),1) = YES;


PARAMETER
LandCover_G4MID(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,*,ScenYear)
LandCover_G4MID_SCEN;







*$GDXIN .\gdx\output_%project%_merged.gdx
*$LOAD LandCover_G4MID_SCEN=LandCover_G4MID
*$GDXIN

FILE Results_SCEN ;
FILE Results_SCEN_out ;

LOOP(AllScenLOOP,

loop (MAP_ScenLOOP_ScenDims(AllScenLOOP,MacroScen,BioenScen,IEA_SCEN,REGION),

 put_utilities Results_SCEN 'gdxin' / '.\gdx\output_SSPxRCP-'AllScenLOOP.tl ;
 execute_load LandCover_G4MID ;

LandCover_G4MID_SCEN(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,"Reserved",ScenYear)
 $ LandCover_G4MID(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,"Reserved",ScenYear)
 = LandCover_G4MID(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,"Reserved",ScenYear);

LandCover_G4MID_SCEN(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear)
 $ LandCover_G4MID(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear)
 = LandCover_G4MID(g4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear);

Option clear = LandCover_G4MID ;
););

execute_unload '.\gdx\LAND_SU_%ssp%_%project%_%lab%.gdx'  LandCover_G4MID_SCEN ;


FILE DownscaledReserved /.\output\output_glo4g4mm_LC_%project%_rel_%lab%.csv/;
PUT  DownscaledReserved;

DownscaledReserved.pw = 1000;
DownscaledReserved.nd = 3;
DownscaledReserved.lw = 0;
DownscaledReserved.nw = 0;


PUT "g4m_05_id,MacroScen,IEA_SCEN,BioenScen";

LOOP(ScenYear,
 PUT ",",ScenYear.TL; );
PUT /;

LOOP((Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen)
 $ SUM(ScenYear,LandCover_G4MID_SCEN(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear)),

 PUT Rg4m_05_id.TL,",",MacroScen.TL,",",IEA_SCEN.TL,",",BioenScen.TL;

 LOOP(ScenYear,
   PUT ",",LandCover_G4MID_SCEN(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"% Reserved",ScenYear));
 PUT /;
     );


FILE DownscaledReservedabs /.\output\output_glo4g4mm_LC_%project%_abs_%lab%.csv/;
PUT  DownscaledReservedabs;

DownscaledReservedabs.pw = 1000;
DownscaledReservedabs.nd = 3;
DownscaledReservedabs.lw = 0;
DownscaledReservedabs.nw = 0;


PUT "g4m_05_id,MacroScen,IEA_SCEN,BioenScen";

LOOP(ScenYear,
 PUT ",",ScenYear.TL; );
PUT /;

LOOP((Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen)
 $ SUM(ScenYear,LandCover_G4MID_SCEN(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"Reserved",ScenYear)),

 PUT Rg4m_05_id.TL,",",MacroScen.TL,",",IEA_SCEN.TL,",",BioenScen.TL;

 LOOP(ScenYear,
   PUT ",",LandCover_G4MID_SCEN(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,"Reserved",ScenYear));
 PUT /;
     );

$exit






* split output file to reduce size of gdx
* 1st output file
FILE Results_SCEN ;
FILE Results_SCEN_out ;

LOOP(AllScenLOOP,

loop (MAP_ScenLOOP_ScenDims(AllScenLOOP,MacroScen,BioenScen,IEA_SCEN,REGION),

 put_utilities Results_SCEN 'gdxin' / '.\gdx\output_trunk-'AllScenLOOP.tl ;
 execute_load LandCover_G4MID ;

 LandCover_G4MID_SCEN(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,LandTypeFAOg4m,ScenYear)=LandCover_G4MID(Rg4m_05_id,MacroScen,IEA_SCEN,BioenScen,LandTypeFAOg4m,ScenYear)

Option clear = LandCover_G4MID ;
););

execute_unload '.\gdx\LAND_SU_msg2_06_%ssp%_%driver%_%region%_%lab%.gdx'  LandCover_G4MID_SCEN ;

