$oneolcom

SETS
unitID            'AUFLS load units'
blockID           'AUFLS blocks'
region            'AUFLS regions'
gxp               'GXP ID'
penaltyThreshold  'Penalty thresholds'
period            'Actual half hours in the relevant historical period'
;

ALIAS (unitID,j,j1), (blockID,b,b1), (region,z,z1)
      (gxp,n,n1), (penaltyThreshold,p,p1), (period,t,t1)
;

SETS
sampledPeriod(t)        'Sampled half hours in the relevant historical period'
j2r(j,z)                'Mapping AUFLS load units to AUFLS regions'
preSelect(j)            'pre-selected AUFLS unit - only apply for clean solve'

;

ALIAS (sampledPeriod,h,h1), (preSelect,pre, pre1)
;

SCALARS
sampledRate             'The total number of sampled half hour periods in relevant historical period / 17520 '
flexibleProportion      'Ensure that [10%] of the target requirements is from flexible AUFLS supply - Extended reserve selection methodology - paragraphs 6b'
additionalFlexible      'provide an additional [10%] of the target requirements as standby (flexible) AUFLS supply - Extended reserve selection methodology - paragraphs 6c'
Xfactor                 'The factor by which ttargets are increased to provide additional flexibleAUFLS supply'
;

PARAMETERS
rop(j)                  'Relay operation payment for AUFLS unit j assigned to block b'
rcp(j,b)                'Relay capital payment for AUFLS unit j assigned to block b'
rfp(j)                  'Relay flexible payment for AUFLS unit j assigned to block b'
duip(j,b)               'Demand unit interruption payement for AUFLS unit j assigned to block b'
rfrp(j)                 'Relay fast response payment for AUFLS unit j assigned to block b'
frc(j)                  'Fast response credit (the credit for relay responding within 250ms) of AUFLS unit j'

regionPenalty(z,p)      'Regional AUFLS deviation penalty'
blockPenalty(z,b,p)     'Regional block AUFLS deviation penalty'
flexPenalty(z,b,p)      'Regional block flexible AUFLS deviation penalty'

regionThreshold(z,p)    'Regional AUFLS penalty thresholds'
blockThreshold(z,b,p)   'Regional block AUFLS penalty thresholds'
PenaltySign(p)          'Penalty sign: 1 --> below threshold, -1 --> above threshold'

blockTarget(z,b)        'Target total AUFLS requirement for each block b in the AUFLS scheme design, expressed as proportions of total demand, in each AUFLS region z'

unitMW(t,j)             'MW load can be used for AUFLS from unit j in half hour h'
regionDemand(t,z)       'the sum of all GXP MW offtakes in AUFLS region z in half hour h'

regionProp(z)           'The sum of all the GXP MWh offtakes in AUFLS region z in all sampled half hours divided by the sum of all the GXP MWh offtakes in the NI all sampled half hours (so that sum over z of region_prop(z)=1)'
;


VARIABLES
SELECTION_COST
ARMING_COST
OPTIMAL_COST
;

BINARY VARIABLES
FLEX(j,b)            'is 1 if AUFLS load unit j is assigned to provide flexible AUFLS for block b'
INFLEX(j,b)          'is 1 if AUFLS load unit j is assigned to provide inflexible AUFLS for block b'
ARMED(j,b)           'is 1 if AUFLS load unit j is armed for AUFLS block b'
;

POSITIVE VARIABLES
BLOCK_MW(h,z,b)      'is total MW of AUFLS allocated to block b in region z for sampled half hour h'
REGION_MW(h,z)       'is total MW of AUFLS allocated to region z for sampled half hour h.'
FLEXIBLE_MW(h,z,b)   'is total MW of AUFLS allocated as flexible AUFLS to block b in region z for sampled half hour h'
STANDBY_MW(h,z,b)    'is total MW of AUFLS allocated as standby AUFLS to block b in region z for sampled half hour h'

BLOCK_DEVIATION(h,z,b,p)      'is deviation of AUFLS block supply/armed from min, max and/or target level in region z during sampled half hour h, %.'
REGION_DEVIATION(h,z,p)       'is deviation of regional AUFLS supply/armed from min, max and/or target level in region z during sampled half hour h, %.'
FLEXIBLE_DEVIATION(h,z,b,p)   'is shortfall of flexible AUFLS block supply from target level in region z during sampled half hour h, %.'
STANDBY_DEVIATION(h,z,b,p)    'is shortfall of standby AUFLS block supply from target level in region z during sampled half hour h, %.'
;

EQUATIONS
Selection_Objective_Function  'Selection objective function'
Arming_Objective_Function     'Arming objective function'

AUFLS_unit_allocation(j)      'AUFLS unit j can be allocated to maximum one block and to be either flexible or inflexible but not both'
AUFLS_unit_arming(j,b)        'AUFLS unit j can be armed for block b only if it was allocated to block b'
AUFLS_unit_must_arm(j,b)      'AUFLS unit j must be armed for block b if it was allocated to block b as inflexible'
AUFLS_unit_preselected(j)     'AUFLS unit j must be allocated for one of the block b it was pre-selected'

Calculate_selected_AUFLS_block_supply(h,z,b)    'Calculating total MW of AUFLS allocated to block b in region z for sampled half hour h'
Calculate_armed_AUFLS_block_supply(h,z,b)       'Calculating total MW of AUFLS armed for block b in region z during sampled half hour h'
Calculate_AUFLS_region_supply(h,z)              'Calculating total MW of AUFLS allocated/armed to region z for sampled half hour h'
Calculate_flexible_AUFLS_block_supply(h,z,b)    'Calculating total MW of flexible AUFLS allocated to block b in region z for sampled half hour h'
Calculate_standby_AUFLS_block_supply(h,z,b)     'Calculating total MW of standby AUFLS allocated to block b in region z for sampled half hour h'

Calculate_AUFLS_block_deviation(h,z,b,p)           'Calculating maximum deviation of armed AUFLS block from min, max or target level in region z, %.'
Calculate_AUFLS_region_deviation(h,z,p)            'Calculating maximum deviation of regional AUFLS supply from min, max or target level in region z, %.'
Calculate_flexible_AUFLS_block_deviation(h,z,b,p)  'Calculating maximum deviation of flexible AUFLS block supply from min level in region z, %.'
Calculate_standby_AUFLS_block_deviation(h,z,b,p)   'Calculating maximum deviation of standby AUFLS block supply from min level in region z, %.'



;


*Choose FLEX(j,b) and INFLEX(j,b) to minimise Obj1 = relay_cost + flex_relay_cost + interruption_cost + AUFLS_penalty + block_penalty + flex_penalty + fast_relay_cost - fast_response_benefit
Selection_Objective_Function..
  SELECTION_COST
=e=
  Sum[ (j,b), rop(j)*(FLEX(j,b) + INFLEX(j,b))                                                                         ]         !! Relay operation cost

+ Sum[ (j,b), rcp(j,b)*(FLEX(j,b) + INFLEX(j,b))                                                                       ]         !! Relay capital cost

+ Sum[ (j,b), rfrp(j)*(FLEX(j,b) + INFLEX(j,b))                                                                        ]         !! Relay fast response cost

+ Sum[ (j,b), rfp(j)*FLEX(j,b)                                                                                         ]         !! Relay flexibility cost

+ Sum[ (j,b), duip(j,b)*INFLEX(j,b) + duip(j,b)*FLEX(j,b)*flexibleProportion/(additionalFlexible + flexibleProportion) ]         !! Interruption cost of selected AUFLS units

- Sum[ (j,b), frc(j)*(FLEX(j,b) + INFLEX(j,b))                                                                         ]         !! Selected AUFLS fast response benefit

+ Sum[ (h,z,p) $ regionPenalty(z,p), 100*regionPenalty(z,p)*REGION_DEVIATION(h,z,p)*regionProp(z)/sampledRate          ]         !! Selected AUFLS region penalty

+ Sum[ (h,z,b,p) $ blockPenalty(z,b,p), 100*blockPenalty(z,b,p)*BLOCK_DEVIATION(h,z,b,p)*regionProp(z)/sampledRate     ]         !! Selected AUFLS block penalty

+ Sum[ (h,z,b,p) $ flexPenalty(z,b,p), 100*flexPenalty(z,b,p)*FLEXIBLE_DEVIATION(h,z,b,p)*regionProp(z)/sampledRate    ]         !! Selected AUFLS flexible block penalty
;


*Given selected FLEX(j,b) and INFLEX(j,b), choose ARMED(j,b)to minimise Obj2 = armed_interruption_cost + AUFLS_penalty + block_penalty - armed_fast_response_benefit
Arming_Objective_Function..
  ARMING_COST
=e=
  Sum[ (j,b), duip(j,b)*ARMED(j,b)                                                                                     ]         !! Interruption cost of armed AUFLS units

- Sum[ (j,b), frc(j)*ARMED(j,b)                                                                                        ]         !! Armed AUFLS fast response benefit

+ Sum[ (h,z,p) $ regionPenalty(z,p), 100*regionPenalty(z,p)*REGION_DEVIATION(h,z,p)*regionProp(z)/sampledRate          ]         !! Armed AUFLS region penalty

+ Sum[ (h,z,b,p) $ blockPenalty(z,b,p), 100*blockPenalty(z,b,p)*BLOCK_DEVIATION(h,z,b,p)*regionProp(z)/sampledRate     ]         !! Armed AUFLS block penalty
;


*AUFLS unit j can be allocated to maximum one block and to be either flexible or inflexible but not both
AUFLS_unit_allocation(j) $ (not preSelect(j))..
  Sum[ b, FLEX(j,b) + INFLEX(j,b) ] =l= 1 ;


*AUFLS unit j must be allocated for one of the block b it was pre-selected
AUFLS_unit_preselected(j) $ preSelect(j)..
  Sum[ b, FLEX(j,b) + INFLEX(j,b) ] =e= 1 ;


*AUFLS unit j can be armed for block b only if it was allocated to block b
AUFLS_unit_arming(j,b)..
  ARMED(j,b) =l= FLEX(j,b) + INFLEX(j,b) ;


*AUFLS unit j must be armed for block b if it was allocated to block b as inflexible
AUFLS_unit_must_arm(j,b)..
  ARMED(j,b) =g= INFLEX(j,b) ;


*Calculating total MW of AUFLS allocated to block b in region z for sampled half hour h
Calculate_selected_AUFLS_block_supply(h,z,b) $ regionDemand(h,z)..
  BLOCK_MW(h,z,b) =e= Sum[ j $ j2r(j,z), unitMW(h,j)*(FLEX(j,b) + INFLEX(j,b)) ] ;


*Calculating total MW of AUFLS armed for block b in region z during sampled half hour h
Calculate_armed_AUFLS_block_supply(h,z,b) $ regionDemand(h,z)..
  BLOCK_MW(h,z,b) =e= Sum[ j $ j2r(j,z), unitMW(h,j) * ARMED(j,b) ] ;


*Calculating total MW of AUFLS armed for region z during sampled half hour h
Calculate_AUFLS_region_supply(h,z) $ regionDemand(h,z)..
  REGION_MW(h,z) =e= Sum[ b, BLOCK_MW(h,z,b) ] ;


*Calculating total MW of flexible AUFLS allocated to block b in region z for sampled half hour h'
Calculate_flexible_AUFLS_block_supply(h,z,b) $ regionDemand(h,z)..
  FLEXIBLE_MW(h,z,b) =e= Sum[ j $ j2r(j,z), unitMW(h,j) * FLEX(j,b) ] ;


*Calculating total MW of AUFLS armed for region z during sampled half hour h
Calculate_standby_AUFLS_block_supply(h,z,b) $ regionDemand(h,z)..
  STANDBY_MW(h,z,b) =e= Sum[ j $ j2r(j,z), unitMW(h,j) * (FLEX(j,b) + INFLEX(j,b) - ARMED(j,b)) ] ;


*Calculating maximum deviation of regional AUFLS supply from min level in region z, %.
Calculate_AUFLS_region_deviation(h,z,p) $ (regionPenalty(z,p) and regionDemand(h,z))..
  REGION_DEVIATION(h,z,p)
=g=
  PenaltySign(p) * [ regionThreshold(z,p)*Xfactor  - REGION_MW(h,z)/regionDemand(h,z) ]
;


*Calculating maximum deviation of AUFLS block supply from min level in region z, %.
Calculate_AUFLS_block_deviation(h,z,b,p) $ (blockPenalty(z,b,p) and regionDemand(h,z))..
  BLOCK_DEVIATION(h,z,b,p)
=g=
  PenaltySign(p) * [ blockThreshold(z,b,p)*Xfactor  - BLOCK_MW(h,z,b)/regionDemand(h,z) ]
;


*Calculating maximum deviation of flexible AUFLS block supply from min level in region z, %.
Calculate_flexible_AUFLS_block_deviation(h,z,b,p) $ (flexPenalty(z,b,p) and regionDemand(h,z))..
  FLEXIBLE_DEVIATION(h,z,b,p)
=g=
  PenaltySign(p) * [ flexibleProportion + additionalFlexible - FLEXIBLE_MW(h,z,b)/[blockTarget(z,b)*regionDemand(h,z)] ]
;


*Calculating maximum deviation of standby AUFLS block supply from min level in region z, %.
Calculate_standby_AUFLS_block_deviation(h,z,b,p) $ (flexPenalty(z,b,p) and regionDemand(h,z))..
  STANDBY_DEVIATION(h,z,b,p)
=g=
  PenaltySign(p) * [ additionalFlexible - STANDBY_MW(h,z,b)/[blockTarget(z,b)*regionDemand(h,z)] ]
;

Model SelectionModel
/
Selection_Objective_Function

AUFLS_unit_allocation
AUFLS_unit_preselected

Calculate_selected_AUFLS_block_supply
Calculate_AUFLS_block_deviation

Calculate_AUFLS_region_supply
Calculate_AUFLS_region_deviation

Calculate_flexible_AUFLS_block_supply
Calculate_flexible_AUFLS_block_deviation
/
;


Model ArmingModel
/
Arming_Objective_Function

Calculate_armed_AUFLS_block_supply
Calculate_AUFLS_block_deviation

Calculate_AUFLS_region_supply
Calculate_AUFLS_region_deviation
/
;
