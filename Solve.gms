$oneolcom

*Set the solver for the LP and MIP
option lp = CPLEX;
option mip = CPLEX;

*Set profile status
option profile = 0;

*Set the solution print status in the lst file
option solprint = off;

*Set the column (variable) and row (equation) listing in the lst file
option limcol = 0;
option limrow = 0;

$include Setting.inc

SET UnitReportItem ''
/
'Currently providing AUFLS'
'Currently allocated block'
'Currently provide flexible'
'Currently armed unit'
'Existing AUFLS functionality'
'Fast response capable'
'Provide flexible service'
'IL proportion'
'Residential share'
'Commercial share'
'Light industrial share'
'Heavy industrial share'
'Sensitive load share'
'Demand weighted Voll, $/MWh'
'Average AUFLS, MW'
'Average demand, MW'
'Allocated block'
'Flexible'
'Armed'
'Standby'
'Installed'
'Tested'
'Reconfigured'
'Relay installation payment'
'Relay testing payment'
'Relay recofiguration payment'
'df/dt relay payment'
'Relay capital payment'
'Relay operation payment'
'Relay fast response payment'
'Relay flexible payment'
'Relay expected interruption cost'
'Total payment'
'Realy expected fast response benefit'
'Demand, MW'
'Armed, MW'
'Flexible, MW'
'Standby, MW'
'Sampled'
'Percent'

/
;


*====Sets and parameters imported from Inputfile ====================================
SETS
unit2gxp(j,n)           'Mapping AUFLS load units to GXP'
gxp2region(n,z)         'Mapping GXP to site to participant and to AUFLS regions'

thresholdAUFLS          'AUFLS thresholds'
penaltyClass            'Penalty classes'
customerClass           'Customer classes'

relayCostComponent      'AUFLS relay cost items'
relayCharacteristics    'AUFLS relay information items'

;

ALIAS (thresholdAUFLS, at), (penaltyClass, pc), (customerClass, cc)
      (relayCostComponent, rc), (relayCharacteristics, ri)

;

PARAMETERS
regionRequirement(z,at)          'AUFLS requirement for region according to Extended reserve technical requirements schedule - paragraph 4'
blockRequirement(z,b,at)         'AUFLS requirement for AUFLS block according to Extended reserve technical requirements schedule - paragraph 5'

unitLoad(t,j)                    'MW load on AUFLS unit j in sampled half hour h'
unitCustomerClassesShare(j,cc)   'Percentage of load from custommer class cc in AUFLS unit j, The special customer is discretionarily assigned demand interruption cost'
unitRelayInformation(j,ri)       'Information of relay at AUFLS unit j'

penaltyFunctionValues(pc,p)      'Penalty values'
expectedInterruptionHours(b)     'Expected interruption hours per year by AUFLS block'
standardInterruptionCost(cc)     'Generic expected interruption cost of  2 hrs unanticipated outage by customer class'
specialInterruptionCost(j)       'Special customer (very large user demand) expected interruption costs for a [2] hr outage in $/MWh'
demandWeightedVoll(j)            'Expected interruption costs for a [2] hr outage in $/MWh'

standardRelayCost(rc)            'Standard AUFLS relay cost compensation rates'
additionalFlexibleCost(j)        'Costs to provide flexible AUFLS supply significantly exceeds the standard flexible AUFLS supply compensation rates'

;


SCALARS
SolveType                  '1 = Clean Solve, 2 = Flexible Solve, 3 = Re-Solve, 4 = Sensitivity Solve'
samplerRate                'The total number of sampled half hour periods in relevant historical period / 17520 '
flexibleProportion         'Ensure that [10%] of the target requirements is from flexible AUFLS supply - Extended reserve selection methodology - paragraphs 6b'
additionalFlexible         'provide an additional [10%] of the target requirements as standby (flexible) AUFLS supply - Extended reserve selection methodology - paragraphs 6c'
standardFastResponseCredit 'Standard Relay Fast Response Benefit rates'
;

$GDXIN %InputFile%.gdx
$LOAD blockID unitID region gxp unit2gxp gxp2region
$LOAD period sampledPeriod thresholdAUFLS penaltyThreshold
$LOAD penaltyClass customerClass relayCostComponent relayCharacteristics
$LOAD regionRequirement blockRequirement
$LOAD unitLoad unitRelayInformation regionDemand
$LOAD penaltyFunctionValues
$LOAD expectedInterruptionHours
$LOAD standardInterruptionCost
$LOAD standardRelayCost
$LOAD flexibleProportion
$LOAD additionalFlexible
$LOAD standardFastResponseCredit
$GDXIN

*====Importing raw data from AUFLS.gdx file end ================================



*====Declare parameters for reporting ==========================================
SCALARS
iter                             'number of binary search tries'        /0/
iterlim                          'max number of binary search tries'    /3/
WhileLimit                       'max number of while iteration'        /0/
;
PARAMETERS
currentProvider(j)               'Flag to indicate if a AUFLF unit is currently providing AUFLS'
currentAllocated(j,b)            'Flag to indicate if a AUFLF unit is currently allocated to block b'
existingFunc(j)                  'Flag to indicate current AUFLS functionality'
currentFlexible(j)               'Flag to indicate if a AUFLF unit is currently allocated as flexible'
currentArmed(j)                  'Flag to indicate if a AUFLF unit is currently armed to block b'
numofRelay(j)                    'Number of relays applied for AUFLS unit j'
fastResponse(j)                  'Flag to indicate if a AUFLF unit is capable of fast resposne'
flexibleCapable(j)               'Flag to indicate if a AUFLF unit is willing to provide flexible service'
ILproportion(j)                  'Estimates of the average alternative demand offered as a proportion of total demand in each relevant time zone'

flexibleUnit(j,b)                'is 1 if AUFLS load unit j is assigned to provide flexible AUFLS for block b'
inflexibleUnit(j,b)              'is 1 if AUFLS load unit j is assigned to provide inflexible AUFLS for block b'
armedUnit(j,b)                   'is 1 if AUFLS load unit j is armed for block b'

allocated(j)                     'Flag to indicate if a AUFLF unit is allocated to block b'
asFlexible(j)                    'Flag to indicate if a AUFLF unit is allocated as flexible'
isArmed(j)                       'Flag to indicate if a AUFLF unit is armed to block b'

blockMW(t,z,b)                   'is total MW of AUFLS allocated to block b in region z for half hour t'
regionMW(t,z)                    'is total MW of AUFLS allocated to region z for half hour t.'
flexibleMW(t,z,b)                'is total MW of AUFLS allocated as flexible AUFLS to block b in region z for half hour t'
standbyMW(t,z,b)                 'is total MW of AUFLS allocated as standby AUFLS to block b in region z for half hour t'

blockDeviation(t,z,b,p)          'is deviation of AUFLS block supply from min, max and/or target level in region z during sampled half hour h, %.'
regionDeviation(t,z,p)           'is deviation of regional AUFLS supply from min, max and/or target level in region z during sampled half hour h, %.'
flexibleDeviation(t,z,b,p)       'is shortfall of flexible AUFLS block supply from target level in region z during sampled half hour h, %.'
standbyDeviation(t,z,b,p)        'is shortfall of standby AUFLS block supply from target level in region z during sampled half hour h, %.'

relayOperationCost(j)               ''
relayCapitalCost(j)                 ''
relayFastResponseCost(j)            ''
relayFlexibilityCost(j)             ''
interruptionCost(j)                 ''

regionDevPenalty(t,z,p)             ''
blockDevPenalty(t,z,b,p)            ''
flexiblePenalty(t,z,b,p)            ''
standbyPenalty(t,z,b,p)             ''

fastResponseBenefit(j)              ''

Total(*)                            ''


blockDeviationProb(z,b,p)        'is deviation of AUFLS armed for block b from min, max and/or target level in region z during sampled half hour h, %.'
regionDeviationProb(z,p)         'is deviation of armed AUFLS in region z from min, max and/or target levelin region z during sampled half hour h, %.'
flexibleDeviationProb(z,b,p)     'is shortfall of flexible AUFLS block supply from target level in region z during sampled half hour h, %.'
standbyDeviationProb(z,b,p)      'is shortfall of standby AUFLS block supply from target level in region z during sampled half hour h, %.'

RelaySummary(j,*)                'Summary of relay selection and arming'
PerformanceSummary(z,*,p)        'Summary of relay selection and arming performance'
RegionSummary(t,z,*)             'Summary of region AUFLS'
BlockSummary(t,z,b,*)            'Summary of block AUFLS'

RegionAUFLSPercent(t,z)          'Actual Region AUFLS by percentage'
RegionMax(t,z)                   '[50] highest AUFLS % in each region.'
RegionMin(t,z)                   '[50] lowest AUFLS % in each region.'
AverageArmedUnitMW(j,z,*)        'Average MW contribution from each armed feeder in periods of [50] highest/lowest regional AUFLS %'
AverageAllUnitMW(j,z,*)          'Average MW contribution from all feeder in periods of [50] highest/lowest regional AUFLS %'

SETS
TimeMax(t,z)                       'periods of [50] highest AUFLS % in each region.'
TimeMin(t,z)                       'periods of [50] lowest AUFLS % in each region.'
;

*====Declare parameters for reporting end ======================================

$If %SolveType% == 'Clean' $Goto SkipLoadingCurrentStatus
*Loading data prom previous sovle if not clean solve
$GDXIN %InputFileX%.gdx
$LOAD flexibleUnit inflexibleUnit armedUnit existingFunc
$GDXIN
$Label SkipLoadingCurrentStatus

$If not %SolveType% == 'Clean' $Goto SkipCleanStart
flexibleUnit(j,b) = 0;
inflexibleUnit(j,b)= 0;
armedUnit(j,b) = 0;
$Label SkipCleanStart

$If %SolveType% == "Clean"    SolveType = 1;
$If %SolveType% == "Flexible" SolveType = 2;
$If %SolveType% == "Resolve"  SolveType = 3;
$If %SolveType% == "Nosolve"  SolveType = 4;
*===== Data preprocessing ======================================================

*Get current AUFLS block if in Re-Solve Mode

currentAllocated(j,b) $ (SolveType = 1) = 1 $ [ord(b) = unitRelayInformation(j,'Currently allocated block')];
currentAllocated(j,b) $ (SolveType > 1) = flexibleUnit(j,b) + inflexibleUnit(j,b);

existingFunc(j) $ (SolveType = 1) = unitRelayInformation(j,'Existing functionality');
*update existing function based on last selection result --> can be deleted later
existingFunc(j) $ (existingFunc(j) = 0) = Sum[ b, flexibleUnit(j,b) + inflexibleUnit(j,b) ]
                                        + Sum[ b1 $ (ord(b1) = 4), flexibleUnit(j,b1) + inflexibleUnit(j,b1) ] ;
existingFunc(j) $ (existingFunc(j) = 1) = 1
                                        + Sum[ b1 $ (ord(b1) = 4), flexibleUnit(j,b1) + inflexibleUnit(j,b1) ] ;
*update existing function based on last selection result end


*This is to make sure that current AUFLS provider must have existing Func > 0
currentProvider(j) = Sum[b, currentAllocated(j,b)] $ existingFunc(j);

currentFlexible(j) = Sum[ b, flexibleUnit(j,b) ] $ (SolveType > 1);

currentArmed(j) = Sum[ b, armedUnit(j,b) ] $ (SolveType > 1);

numofRelay(j) = unitRelayInformation(j,'Number of relay');

fastResponse(j) = unitRelayInformation(j,'Fast resposnse capable');

flexibleCapable(j) = unitRelayInformation(j,'Can provide flexible service');

ILproportion(j) = unitRelayInformation(j,'IL proportion');

preSelect(j) = no;

j2r(j,z) = yes $ Sum[ n $ (unit2gxp(j,n) and gxp2region(n,z)), 1];

*PARAMETERS
sampledRate = card(h)/card(t);

*MW load can be used for AUFLS from unit j in half hour h
unitMW(t,j) = unitLoad(t,j) * [ 1 - ILproportion(j) ];

*Relay operation payment for AUFLS unit j - standard relay administration cost $/year
rop(j) = standardRelayCost('Administration cost') ;

*Calulate relay capital payment for AUFLS unit j assigned to block b
rcp(j,b) = ( [ standardRelayCost('Base capital cost')           $ ((ord(b) < 4) and (existingFunc(j)=0)) ]
           + [ standardRelayCost('Testing cost')                $ ((ord(b) < 4) and (currentProvider(j)=0)) ]
           + [ standardRelayCost('Reconfiguring cost')          $ ((ord(b) < 4) and (existingFunc(j)>0)) ]
           + [ standardRelayCost('Base capital cost')           $ ((ord(b) = 4) and (existingFunc(j)<2)) ]
           + [ standardRelayCost('Testing cost')                $  (ord(b) = 4) ]
           + [ standardRelayCost('Reconfiguring cost')          $ ((ord(b) = 4) and (existingFunc(j)=2)) ]
           + [ standardRelayCost('Additional df/dt relay cost') $  (ord(b) = 4) ]
           ) ;
rcp(j,b) $ (currentAllocated(j,b) and (SolveType > 1)) = 0 ;

*Relay fast response payment for AUFLS unit j assigned to block b
rfrp(j) = standardRelayCost('Fast response cost') $ fastResponse(j);

*Fast response credit (the credit for relay responding within 250ms) of AUFLS unit j
frc(j) = standardFastResponseCredit $ fastResponse(j);

*Relay flexible payment for AUFLS unit j assigned to block b as flexible
rfp(j) = standardRelayCost('Flexible service cost');

*Expected interruption costs for a [2] hr outage in $/MWh - Extended reserve selection methodology - Schedule 1 item 14
unitCustomerClassesShare(j,'Residential') = unitRelayInformation(j,'Residential');
unitCustomerClassesShare(j,'Small NR') = unitRelayInformation(j,'Small NR');
unitCustomerClassesShare(j,'Medium NR') = unitRelayInformation(j,'Medium NR');
unitCustomerClassesShare(j,'Large NRl') = unitRelayInformation(j,'Large NRl');
unitCustomerClassesShare(j,'High Sensitive') = unitRelayInformation(j,'High Sensitive');

demandWeightedVoll(j)
  = Sum[ cc, standardInterruptionCost(cc) * unitCustomerClassesShare(j,cc) ]
;

*Demand unit interruption payement for AUFLS unit j assigned to block b
duip(j,b) = demandWeightedVoll(j)                     !! Expected interruption costs forr [2] hr outage in $/MWh
          * expectedInterruptionHours(b)              !! Expected interruption costs forr a [2] hr outage in $/MWh
          * Sum[ t, unitMW(t,j)] / card(t)            !! The average net demand in MW
;

*Regional AUFLS deviation penalty values
regionPenalty(z,p) = sum[ pc $ (ord(pc) = 1), penaltyFunctionValues(pc,p) ];

*Regional block AUFLS deviation penalty values
blockPenalty(z,b,p) = sum[ pc $ (ord(pc) = 2), penaltyFunctionValues(pc,p) ];

*Regional block flexible AUFLS deviation penalty value
flexPenalty(z,b,p) $ [(ord(p) >= 2) and (ord(p) <= 3)] = sum[ pc $ (ord(pc) = 3), penaltyFunctionValues(pc,p) ] ;

*Regional AUFLS penalty thresholds
regionThreshold(z,p) $ (ord(p) <= 2) = sum[ at $ (ord(at) = ord(p)), regionRequirement(z,at) ];
regionThreshold(z,p) $ (ord(p) >= 3) = sum[ at $ (ord(at) = ord(p)-1), regionRequirement(z,at) ];

*Regional block AUFLS penalty thresholds
blockThreshold(z,b,p) $ (ord(p) <= 2) = sum[ at $ (ord(at) = ord(p)), blockRequirement(z,b,at) ];
blockThreshold(z,b,p) $ (ord(p) >= 3) = sum[ at $ (ord(at) = ord(p)-1), blockRequirement(z,b,at) ];

*Penalty sign: 1 --> below threshold, -1 --> above threshold
PenaltySign(p) = 1 $ (ord(p) <= 2)
               - 1 $ (ord(p) >= 3)
;

*Target total AUFLS requirement for each block b in the AUFLS scheme design, expressed as proportions of total demand, in each AUFLS region z
blockTarget(z,b) = blockRequirement(z,b,'target');

*The sum of all the GXP MWh offtakes in AUFLS region z in all sampled half hours divided by the sum of all the GXP MWh offtakes in the NI all sampled half hours (so that sum over z of region_prop(z)=1)
regionProp(z) = Sum[ h, regionDemand(h,z) ] / Sum[ (h,z1), regionDemand(h,z1) ]
;

*===== Data preprocessing end ==================================================

*execute_unload '%OutputFile%';
*$Stop

*===== SOLVING MODELS ==========================================================
$If %SolveType% == "Nosolve"  $Goto Reporting

*Solving selection process model +++++++++++++++++++++++++++++++++++++++++++++++
For [ iter = 1 to iterlim,
   if [ (SolveType = 2),
      iter = iterlim;

   else
      FLEX.l(j,b) = flexibleUnit(j,b);
      FLEX.fx(j,b) $ (not flexibleCapable(j)) = 0;
      INFLEX.l(j,b) = inflexibleUnit(j,b);
      Option Clear = flexibleUnit;
      Option Clear = inflexibleUnit;
      Option Clear = blockMW;
      Option Clear = regionMW;
      Option Clear = regionDeviation;
      Option Clear = regionDeviationProb;

*     Set the optimality criteria for the MIP
      SelectionModel.OptFile = 1;
      SelectionModel.reslim = 40000;
      SelectionModel.iterlim = 1000000;
      SelectionModel.cheat = 1000;
      Xfactor = 1 + additionalFlexible;
*     Solve selection model
      solve SelectionModel using mip minimizing SELECTION_COST;

      if [ ((SelectionModel.modelstat = 1) or (SelectionModel.modelstat = 8)),
         flexibleUnit(j,b) = round(FLEX.l(j,b));
         inflexibleUnit(j,b) = round(INFLEX.l(j,b));
      ] ;
   ] ;

   ARMED.l(j,b) = armedUnit(j,b);
   ARMED.fx(j,b) $ inflexibleUnit(j,b) = 1;
   ARMED.up(j,b) = inflexibleUnit(j,b) + flexibleUnit(j,b);
   Option Clear = armedUnit;
   Option Clear = blockMW;
   Option Clear = regionMW;
   Option Clear = regionDeviation;
   Option Clear = regionDeviationProb;

*  Set the optimality criteria for the MIP
   ArmingModel.optcr = 0;
   ArmingModel.reslim = 10000;
   ArmingModel.iterlim = 1000000;
   Xfactor = 1;
*  Solve the model
   solve ArmingModel using mip minimizing ARMING_COST;

   if [ ((ArmingModel.modelstat = 1) or (ArmingModel.modelstat = 8)),

*     Testing region below min probability -------------------------------------
      armedUnit(j,b) = round(ARMED.l(j,b));

      blockMW(t,z,b) = Sum[ j $ j2r(j,z), armedUnit(j,b) * unitMW(t,j) ] ;
      regionMW(t,z) = Sum[ b, blockMW(t,z,b) ] ;
      regionDeviation(t,z,p) = Max[ 0, PenaltySign(p)*(regionThreshold(z,p)*Xfactor - regionMW(t,z)/regionDemand(t,z)) ] $ [regionDemand(t,z) > 0] ;
      regionDeviationProb(z,p) = Sum[ t $ regionDeviation(t,z,p), 1 ] / card(t);

      display iter, regionPenalty, regionDeviationProb;
      iter $ (Sum[ (z,p) $ [(ord(p) = 1) and (regionDeviationProb(z,p) > 0.1)], 1 ] = 0) = iterlim ;
      regionPenalty(z,p) $ (ord(p) = 1) = regionPenalty(z,p) + [100 $  (iter < iterlim)] ;
      display iter, regionPenalty;
*     Testing region below min probability end ---------------------------------

   else
*Infeasible or no solution occurs --> stop
      iter = iterlim ;
   ] ;

] ;


$Label Reporting
Xfactor = 1;
allocated(j) = Sum[ b, ord(b) * (flexibleUnit(j,b) + inflexibleUnit(j,b)) ] ;
asFlexible(j) = Sum[ b, flexibleUnit(j,b) ] ;
isArmed(j) = Sum[ b, armedUnit(j,b) ] ;

blockMW(t,z,b) = Sum[ j $ j2r(j,z), armedUnit(j,b) * unitMW(t,j) ] ;
regionMW(t,z) = Sum[ b, blockMW(t,z,b) ] ;
flexibleMW(t,z,b) = Sum[ j $ j2r(j,z), flexibleUnit(j,b) * unitMW(t,j) ] ;
standbyMW(t,z,b) = Sum[ j $ j2r(j,z), (flexibleUnit(j,b) + inflexibleUnit(j,b) - armedUnit(j,b)) * unitMW(t,j) ] ;

blockDeviation(t,z,b,p) = Max[ 0, PenaltySign(p) * ( blockThreshold(z,b,p)*Xfactor - blockMW(t,z,b)/regionDemand(t,z) ) ] $ [regionDemand(t,z) > 0] ;
regionDeviation(t,z,p) = Max[ 0, PenaltySign(p) * ( regionThreshold(z,p)*Xfactor - regionMW(t,z)/regionDemand(t,z) ) ] $ [regionDemand(t,z) > 0] ;
flexibleDeviation(t,z,b,p) = Max[ 0, PenaltySign(p) * ( flexibleProportion + additionalFlexible - flexibleMW(t,z,b)/[blockTarget(z,b)*regionDemand(t,z)] ) ] $ [(ord(p) > 1) and (ord(p) <4) and (regionDemand(t,z) > 0)] ;
standbyDeviation(t,z,b,p) = Max[ 0, PenaltySign(p) * ( additionalFlexible - standbyMW(t,z,b)/[blockTarget(z,b)*regionDemand(t,z)] ) ] $ [(ord(p) > 1) and (ord(p) <4) and (regionDemand(t,z) > 0)] ;

blockDeviationProb(z,b,p) = Sum[ t $ blockDeviation(t,z,b,p), 1 ] / card(t);
regionDeviationProb(z,p) = Sum[ t $ regionDeviation(t,z,p), 1 ] / card(t);
flexibleDeviationProb(z,b,p) = Sum[ t $ flexibleDeviation(t,z,b,p), 1 ] / card(t);
standbyDeviationProb(z,b,p) = Sum[ t $ standbyDeviation(t,z,b,p), 1 ] / card(t);

regionDevPenalty(t,z,p) =  100 * regionPenalty(z,p) * regionDeviation(t,z,p) * regionProp(z) ;
blockDevPenalty(t,z,b,p) = 100 * blockPenalty(z,b,p) * blockDeviation(t,z,b,p) * regionProp(z) ;
flexiblePenalty(t,z,b,p) = 100 * flexPenalty(z,b,p) * flexibleDeviation(t,z,b,p) * regionProp(z) ;
standbyPenalty(t,z,b,p) = 100 * flexPenalty(z,b,p) * standbyDeviation(t,z,b,p) * regionProp(z) ;

relayOperationCost(j) = Sum[ b, rop(j) * (flexibleUnit(j,b) + inflexibleUnit(j,b)) ] ;
relayCapitalCost(j) = Sum[ b, rcp(j,b) * (flexibleUnit(j,b) + inflexibleUnit(j,b)) ] ;
relayFastResponseCost(j) = Sum[ b, rfrp(j) * (flexibleUnit(j,b) + inflexibleUnit(j,b)) ] ;
relayFlexibilityCost(j) = Sum[ b, rfp(j) * flexibleUnit(j,b) ] ;
interruptionCost(j) = Sum[ b, duip(j,b) * (flexibleUnit(j,b) + inflexibleUnit(j,b)) ] ;
fastResponseBenefit(j) = Sum[ b, frc(j) * armedUnit(j,b) ] ;

Total('Total Cost') = Sum[ j , relayOperationCost(j) + relayCapitalCost(j) + relayFastResponseCost(j) + relayFlexibilityCost(j) + interruptionCost(j) - 0*fastResponseBenefit(j) ] ;
Total('Penalty') = Sum[ (t,z,p), regionDevPenalty(t,z,p) ] + Sum[ (t,z,b,p), blockDevPenalty(t,z,b,p) ] + Sum[ (t,z,b,p), flexiblePenalty(t,z,b,p) ] + 0*Sum[ (t,z,b,p), standbyPenalty(t,z,b,p) ] ;

*Summary of relay selection and arming
RelaySummary(j,'Currently providing AUFLS') = currentProvider(j) + eps;
RelaySummary(j,'Currently allocated block') = Sum[ b, ord(b) * currentAllocated(j,b)] + eps;
RelaySummary(j,'Currently provide flexible') = currentFlexible(j) + eps;
RelaySummary(j,'Currently armed unit') = currentArmed(j) + eps;
RelaySummary(j,'Existing AUFLS functionality') = existingFunc(j) + eps ;
RelaySummary(j,'Fast response capable') = fastResponse(j) + eps;
RelaySummary(j,'Provide flexible service') = flexibleCapable(j) + eps;
RelaySummary(j,'IL proportion') = ILproportion(j) + eps;
RelaySummary(j,'Residential share') = unitCustomerClassesShare(j,'Residential') + eps;
RelaySummary(j,'Commercial share') = unitCustomerClassesShare(j,'Small NR') + eps;
RelaySummary(j,'Light industrial share') = unitCustomerClassesShare(j,'Medium NR') + eps;
RelaySummary(j,'Heavy industrial share') = unitCustomerClassesShare(j,'Large NRl') + eps;
RelaySummary(j,'Sensitive load share') = unitCustomerClassesShare(j,'High Sensitive') + eps;
RelaySummary(j,'Demand weighted Voll, $/MWh') = demandWeightedVoll(j) + eps ;
RelaySummary(j,'Average AUFLS, MW') = Sum[ t, unitMW(t,j)] / card(t)  + eps;
RelaySummary(j,'Average demand, MW') = Sum[ t, unitLoad(t,j)] / card(t)  + eps;
RelaySummary(j,'Allocated block') = allocated(j)  + eps;
RelaySummary(j,'Flexible') = asFlexible(j) + eps;
RelaySummary(j,'Armed') = isArmed(j) + eps;
RelaySummary(j,'Standby') = eps + [1 $ (asFlexible(j)>isArmed(j))] ;
RelaySummary(j,'Installed') = Sum[ b $ ((ord(b) < 4) and (existingFunc(j)=0)), (flexibleUnit(j,b) + inflexibleUnit(j,b)) * (1-currentAllocated(j,b)) ]
                            + Sum[ b $ ((ord(b) = 4) and (existingFunc(j)<2)), (flexibleUnit(j,b) + inflexibleUnit(j,b)) * (1-currentAllocated(j,b)) ]
                            + eps;
RelaySummary(j,'Reconfigured') = Sum[ b $ ((ord(b) < 4) and (existingFunc(j)>0)), (flexibleUnit(j,b) + inflexibleUnit(j,b)) * (1-currentAllocated(j,b))  ]
                               + Sum[ b $ ((ord(b) = 4) and (existingFunc(j)=2)), (flexibleUnit(j,b) + inflexibleUnit(j,b)) * (1-currentAllocated(j,b))  ]
                               + eps;
RelaySummary(j,'Tested') = Sum[ b $ ((ord(b) < 4) and (currentProvider(j)=0)), (flexibleUnit(j,b) + inflexibleUnit(j,b)) * (1-currentAllocated(j,b)) ]
                         + Sum[ b $ (ord(b) = 4), (flexibleUnit(j,b) + inflexibleUnit(j,b)) * (1-currentAllocated(j,b)) ]
                         + eps;
RelaySummary(j,'Relay installation payment') = RelaySummary(j,'Installed') * standardRelayCost('Base capital cost') + eps;
RelaySummary(j,'Relay testing payment') = RelaySummary(j,'Tested') * standardRelayCost('Testing cost') + eps;
RelaySummary(j,'Relay recofiguration payment') = RelaySummary(j,'Reconfigured') * standardRelayCost('Reconfiguring cost') + eps;
RelaySummary(j,'df/dt relay payment') = Sum[ b $ (ord(b) = 4), standardRelayCost('Additional df/dt relay cost')
                                                             * (flexibleUnit(j,b) + inflexibleUnit(j,b))
                                                             * (1-currentAllocated(j,b))
                                           ] + eps;
RelaySummary(j,'Relay capital payment') = relayCapitalCost(j) + eps;
RelaySummary(j,'Relay operation payment') = relayOperationCost(j) + eps;
RelaySummary(j,'Relay fast response payment') = relayFastResponseCost(j) + eps;
RelaySummary(j,'Relay flexible payment') = relayFlexibilityCost(j) + eps;
RelaySummary(j,'Relay expected interruption cost') = interruptionCost(j) + eps;
RelaySummary(j,'Total payment') =  relayCapitalCost(j) + relayOperationCost(j) + relayFastResponseCost(j) + relayFlexibilityCost(j) + interruptionCost(j) + eps;
RelaySummary(j,'Realy expected fast response benefit') = fastResponseBenefit(j) + eps;

*Summary of relay selection and arming performance
PerformanceSummary(z,'Region',p) = regionDeviationProb(z,p);
PerformanceSummary(z,'Block 1',p) = Sum[ b $ (ord(b) = 1), blockDeviationProb(z,b,p) ];
PerformanceSummary(z,'Block 2',p) = Sum[ b $ (ord(b) = 2), blockDeviationProb(z,b,p) ];
PerformanceSummary(z,'Block 3',p) = Sum[ b $ (ord(b) = 3), blockDeviationProb(z,b,p) ];
PerformanceSummary(z,'Block 4',p) = Sum[ b $ (ord(b) = 4), blockDeviationProb(z,b,p) ];
PerformanceSummary(z,'Flexi 1',p) = Sum[ b $ (ord(b) = 1),flexibleDeviationProb(z,b,p) ];
PerformanceSummary(z,'Flexi 2',p) = Sum[ b $ (ord(b) = 2),flexibleDeviationProb(z,b,p) ];
PerformanceSummary(z,'Flexi 3',p) = Sum[ b $ (ord(b) = 3),flexibleDeviationProb(z,b,p) ];
PerformanceSummary(z,'Flexi 4',p) = Sum[ b $ (ord(b) = 4),flexibleDeviationProb(z,b,p) ];
PerformanceSummary(z,'Stanby 1',p) = Sum[ b $ (ord(b) = 1), standbyDeviationProb(z,b,p) ];
PerformanceSummary(z,'Stanby 2',p) = Sum[ b $ (ord(b) = 2), standbyDeviationProb(z,b,p) ];
PerformanceSummary(z,'Stanby 3',p) = Sum[ b $ (ord(b) = 3), standbyDeviationProb(z,b,p) ];
PerformanceSummary(z,'Stanby 4',p) = Sum[ b $ (ord(b) = 4), standbyDeviationProb(z,b,p) ];

*Summary of region AUFLS
RegionSummary(t,z,'Demand, MW') = regionDemand(t,z);
RegionSummary(t,z,'Armed, MW') = regionMW(t,z) + eps;
RegionSummary(t,z,'Sampled') = [1$sampledPeriod(t)] + eps;
RegionSummary(t,z,'Percent') = [regionMW(t,z)/regionDemand(t,z)] $ regionDemand(t,z);

*Summary of block AUFLS
BlockSummary(t,z,b,'Armed, MW') = blockMW(t,z,b);
BlockSummary(t,z,b,'Flexible, MW') = flexibleMW(t,z,b);
BlockSummary(t,z,b,'Standby, MW') = standbyMW(t,z,b);

*Update existing functionality
existingFunc(j) $ (existingFunc(j) = 0) = Sum[ b, flexibleUnit(j,b) + inflexibleUnit(j,b) ] + Sum[ b1 $ (ord(b1) = 4), flexibleUnit(j,b1) + inflexibleUnit(j,b1) ] ;
existingFunc(j) $ (existingFunc(j) = 1) = 1 + Sum[ b1 $ (ord(b1) = 4), flexibleUnit(j,b1) + inflexibleUnit(j,b1) ] ;

RegionAUFLSPercent(t,z) = [regionMW(t,z)/regionDemand(t,z)] $ [regionDemand(t,z) > 0];
RegionMax(h,z) = sum[ h1 $ (RegionAUFLSPercent(h1,z)>=RegionAUFLSPercent(h,z)), 1 ] ;
TimeMax(h,z) = yes $ (RegionMax(h,z)<=50);
TimeMin(h,z) = yes $ (RegionMax(h,z)>=(card(h)-49));
AverageArmedUnitMW(j,z,'50RegionMax') = Sum[ (h,b) $ ( j2r(j,z) and TimeMax(h,z) ), armedUnit(j,b) * unitMW(h,j) ] / Sum[ h $ TimeMax(h,z), 1] ;
AverageArmedUnitMW(j,z,'50RegionMin') = Sum[ (h,b) $ ( j2r(j,z) and TimeMin(h,z) ), armedUnit(j,b) * unitMW(h,j) ] / Sum[ h $ TimeMin(h,z), 1] ;
AverageAllUnitMW(j,z,'50RegionMax') = Sum[ h $ ( j2r(j,z) and TimeMax(h,z) ), unitMW(h,j) ] / Sum[ h $ TimeMax(h,z), 1] ;
AverageAllUnitMW(j,z,'50RegionMin') = Sum[ h $ ( j2r(j,z) and TimeMin(h,z) ), unitMW(h,j) ] / Sum[ h $ TimeMin(h,z), 1] ;


execute_unload "%OutputFile%",
   currentProvider
   currentAllocated
   existingFunc
   currentFlexible
   currentArmed
   numofRelay
   fastResponse
   flexibleCapable
   ILproportion

   flexibleUnit
   inflexibleUnit
   armedUnit

   allocated
   asFlexible
   isArmed

   blockMW
   regionMW
   flexibleMW
   standbyMW

   regionDeviation
   blockDeviation
   flexibleDeviation
   standbyDeviation

   relayOperationCost
   relayCapitalCost
   relayFastResponseCost
   relayFlexibilityCost
   interruptionCost
   fastResponseBenefit

   regionDevPenalty
   blockDevPenalty
   flexiblePenalty
   standbyPenalty

   regionDeviationProb
   blockDeviationProb
   flexibleDeviationProb
   standbyDeviationProb

   RelaySummary
   PerformanceSummary
   RegionSummary
   BlockSummary
   Total

   RegionAUFLSPercent
   RegionMax
   TimeMax
   TimeMin
   AverageArmedUnitMW
   AverageAllUnitMW
;

*Solving AUFLS arming model end +++++++++++++++++++++++++++++++++++++++++++++++
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx set=TimeMax Rng=TimeMax!A1 rdim=1 cdim=1"
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx set=TimeMin Rng=TimeMin!A1 rdim=1 cdim=1"
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx par=AverageArmedUnitMW Rng=AverageArmedUnitMW!A1"
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx par=AverageAllUnitMW Rng=AverageAllUnitMW!A1"

$Stop

execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx par=RelaySummary Rng=Unit!A1"
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx par=PerformanceSummary Rng=Performance!A1"
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx par=RegionSummary Rng=Region!A1"
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx par=BlockSummary Rng=Block!A1"
execute "Gdxxrw %InputFile%.gdx o=%OutputFile%.xlsx set=sampledPeriod Rng=SampledHlafHours!A1 rdim=1"
execute "Gdxxrw %OutputFile%.gdx o=%OutputFile%.xlsx par=Total Rng=Total!A1"
