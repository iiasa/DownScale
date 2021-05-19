SETS
AllScenLOOP /0*20000/

SimUID/1*212707/

LC_TYPES_EPIC/CrpLnd, Grass, Forest, PltFor, OthNatLnd, SimUarea/

LandTypeFAO/ArbLndPls, GrsLndTmp, GrsLndPrm, GrsLndTot, PltForTot, ForLndTot, OthLndTot, TotLnd, TotLndnew, Reserved/

LUC_Set /CrpLnd, GrsLnd, NatLnd, PltFor, PriFor, MngFor/

LC_MAP_EPIC_LUCSET(LC_TYPES_EPIC,LUC_Set)
/
 CrpLnd      .  CrpLnd
 Grass       .  GrsLnd
 Forest      .  PriFor
* Forest      .  (PriFor, MngFor)
 PltFor      .  PltFor
 OthNatLnd   .  NatLnd
* SimUarea    .  SimUarea
/

LandTypeFAOg4m(LandTypeFAO) /Reserved, TotLnd, TotLndnew/

LandType_MAP(LC_TYPES_EPIC,LandTypeFAO)/
 CrpLnd      .  ArbLndPls
 Grass       .  GrsLndTot
 Forest      .  ForLndTot
 PltFor      .  PltForTot
 OthNatLnd   .  OthLndTot
 SimUarea    .  TotLnd
(CrpLnd, Grass, PltFor, Forest, OthNatLnd).  TotLndnew
(CrpLnd, Grass, PltFor)     . Reserved
/

CROP/
Barl
BeaD
Cass
ChkP
Corn
Cott
Gnut
Mill
Pota
Rape
Rice
Soya
Srgh
SugC
Sunf
SwPo
Whea
OPAL
banp
bean
opul
sugb
coff
ofib
grou
ooil
othe/
*opul

INPUT_LEVEL/
HI
LI
IR
SS/

ATTRIBUTE1 /VALUE, COUNT1, AREA1, MIN1, MAX1, RANGE1, MEAN1, STD1, SUM1/
ATTRIBUTE2 /VALUE2, COUNT2, AREA2, MEAN2000, MEAN2010, MEAN2020, MEAN2030, MEAN2040, MEAN2050/
SOL / Init, Fin, Perc /
;

SET
LC_MAP_EPIC_LUCSET2(LC_TYPES_EPIC,LUC_Set);
LC_MAP_EPIC_LUCSET2(LC_TYPES_EPIC,LUC_Set)
 $ LC_MAP_EPIC_LUCSET(LC_TYPES_EPIC,LUC_Set) = YES ;

SET regr_coeff_exist(LC_TYPES_EPIC,LC_TYPES_EPIC) /
  CrpLnd.Grass
  CrpLnd.OthNatLnd
  Grass.CrpLnd
  Grass.OthNatLnd
  Forest.CrpLnd
  Forest.Grass
  OthNatLnd.CrpLnd
  OthNatLnd.Grass
/;


ALIAS(LC_TYPES_EPIC,LC_TYPES_EPIC1,LC_TYPES_EPIC2);
ALIAS(LUC_Set,LUC1);
ALIAS(LUC_Set,LUC_Set1);
ALIAS(LUC_Set,LUC2);
ALIAS(LUC_Set,LUC_Set2);
ALIAS(INPUT_LEVEL,l);
ALIAS(CROP,k);








