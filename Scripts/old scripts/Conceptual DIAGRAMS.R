
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(ggplot2)
library(ggrepel)


# Create conceptual diagram for fire 
savanna_fire_diagram <- grViz("
digraph savanna_fire {

  graph [layout = dot, rankdir = TB]

  node [
    shape = rectangle,
    style = filled,
    fillcolor = '#F7F7F7',
    color = black,
    fontname = Helvetica
  ]

  # Context
  Climate [label = 'Climate & Context\\n(Rainfall)', fillcolor = '#D9EAD3']
  Grass   [label = 'Grass biomass\\n(Fuel load)', fillcolor = '#EAD1DC']
  Fire    [label = 'Fire regime\\n(Frequency × Intensity × Season)', fillcolor = '#F4CCCC']

  # Juvenile pathways
  Seedling [label = 'Seedling mortality\\n(Establishment failure)', fillcolor = '#FFF2CC']
  Topkill  [label = 'Sapling topkill\\n& resprouting cycle', fillcolor = '#FFF2CC']
  FireTrap [label = 'Fire trap\\n(Height suppression,\\nNon-structural carbohydrates)', fillcolor = '#FFE599']

  # Outcomes
  EscapeFail [label = 'Failure to escape\\ninto adult size classes', fillcolor = '#D0E0E3']
  Densification [label = 'Suppress bush\\nencroachment', fillcolor = '#C9DAF8']

  # Feedbacks
  GrassFB [label = 'Fire–grass feedback\\n(Open canopy → more grass → more fire)',
           fillcolor = '#EAD1DC']
  HerbFB  [label = 'Fire–herbivory feedback\\n(Browsing of resprouts)',
           fillcolor = '#D9D2E9']

  
  # Edges with signs
  Climate -> Grass   [label = '+']
  Grass   -> Fire    [label = '+']

  Fire -> Seedling   [label = '+']
  Fire -> Topkill    [label = '+']

  Seedling -> EscapeFail [label = '+']
  Topkill  -> FireTrap   [label = '+']
  FireTrap -> EscapeFail [label = '+']

  EscapeFail -> Densification 

  Densification -> GrassFB [label = '+']
  GrassFB -> Grass        [label = '+']

  Fire -> HerbFB   [label = '+']
  HerbFB -> FireTrap [label = '+']
}
")

# Export as PNG
# Convert DiagrammeR object to SVG
svg <- export_svg(savanna_fire_diagram)

# Write PNG file
rsvg_png(
  charToRaw(svg),
  file = "Plots/2Conceptual_synthesis_fire_woody_dynamics.png",
  width = 2000,   # pixels
  height = 2600   # pixels
)





##########################################################################

################### using two colours only dark green and light green

savanna_fire_diagram <- grViz("
digraph savanna_fire {

  graph [layout = dot, rankdir = TB]

  node [
    shape = rectangle,
    style = filled,
    color = black,
    fontname = Helvetica
  ]

  # Dark green = promotes woody densification
  Climate [label = 'Climate & Context\\n(Rainfall)',
           fillcolor = '#1B5E20', fontcolor = white]

  Grass [label = 'Grass biomass\\n(Fuel load)',
         fillcolor = '#1B5E20', fontcolor = white]

  GrassFB [label = 'Fire–grass feedback\\n(Open canopy → more grass → more fire)',
           fillcolor = '#1B5E20', fontcolor = white]

  # Light green = reduction / suppression of woody plants
  Fire [label = 'Fire regime\\n(Frequency × Intensity × Season)',
        fillcolor = '#A5D6A7']

  Seedling [label = 'Seedling mortality\\n(Establishment failure)',
            fillcolor = '#A5D6A7']

  Topkill [label = 'Juvenile topkill\\n& resprouting cycle',
           fillcolor = '#A5D6A7']

  FireTrap [label = 'Fire trap\\n(Height suppression,\\nNon-structural carbohydrate depletion)',
            fillcolor = '#A5D6A7']

  EscapeFail [label = 'Failure to escape\\ninto adult size classes',
              fillcolor = '#A5D6A7']

  Densification [label = 'Suppress bush\\nencroachment',
                 fillcolor = '#A5D6A7']

  HerbFB [label = 'Fire–herbivory feedback\\n(Browsing of resprouts)',
          fillcolor = '#A5D6A7']

  # Edges with + / – signs
  Climate -> Grass   [label = '(+)']
  Grass   -> Fire    [label = '(+)']

  Fire -> Seedling   [label = '(+)']
  Fire -> Topkill    [label = '(+)']

  Seedling -> EscapeFail [label = '(+)']
  Topkill  -> FireTrap   [label = '(+)']
  FireTrap -> EscapeFail [label = '(+)']

  EscapeFail -> Densification [label = '(–)']

  Densification -> GrassFB [label = '(+)']
  GrassFB -> Grass        [label = '(+)']

  Fire -> HerbFB      [label = '(+)']
  HerbFB -> FireTrap  [label = '(+)']
}
")

# Export as PNG
# Convert DiagrammeR object to SVG
svg2 <- export_svg(savanna_fire_diagram)

# Write PNG file
#rsvg_png(
  charToRaw(svg2),
  file = "Plots/FireConceptual_woody_dynamics.png",
  width = 2000,   # pixels
  height = 2600   # pixels
)


############################# Conceptual Framework FIRE - no colours


firenocolours <- grViz("
digraph savanna_fire {

  graph [layout = dot, rankdir = TB]

 

node [shape = rectangle, style = filled, fillcolor = white, color = black, fontname = Helvetica]

  # Dark green = promotes woody densification
  Climate [label = 'Climate\\n(Rainfall)',
            fontcolor = black]

  Grass [label = 'Grass biomass\\n(Fuel load)',
          fontcolor = black]

  GrassFB [label = 'Fire–grass feedback\\n(Open canopy → more grass → more fire)',
           fontcolor = black]

  # Light green = reduction / suppression of woody plants
  Fire [label = 'Fire regime\\n(Frequency × Intensity × Season)',
       ]

  Seedling [label = 'Seedling mortality\\n(Establishment failure)',
            ]

  Topkill [label = 'Juvenile topkill\\n& resprouting cycle',
          ]

  FireTrap [label = 'Fire trap\\n(Height suppression,\\nNon-structural carbohydrate depletion)',
            ]

  EscapeFail [label = 'Failure to escape\\ninto mature size classes',
             ]

  Densification [label = 'Bush\\nencroachment',
                 ]

  # Edges with + / – signs
  Climate -> Grass   [label = '(+)']
  Grass   -> Fire    [label = '(+)']

  Fire -> Seedling   [label = '(+)']
  Fire -> Topkill    [label = '(+)']

  Seedling -> EscapeFail [label = '(+)']
  Topkill  -> FireTrap   [label = '(+)']
  FireTrap -> EscapeFail [label = '(+)']

  EscapeFail -> Densification [label = '(–)']

  Densification -> GrassFB [label = '(+)']
  GrassFB -> Grass        [label = '(+)']

  
}
")

# Export as PNG
# Convert DiagrammeR object to SVG
fireNC <- export_svg(firenocolours)

# Write PNG file
rsvg_png(
charToRaw(fireNC),
file = "Plots/FireConceptual_woody_dynamics.png",
width = 2000,   # pixels
height = 2600   # pixels
)


################### FIRE - REFINED VERSION TO REMOVE NON-NEUTRAL WORDS

fireC2 <- grViz("
digraph savanna_fire {

  graph [layout = dot, rankdir = TB]

node [shape = rectangle, style = filled, fillcolor = white, color = black, fontname = Helvetica]

  # Dark green = promotes woody densification
  Climate [label = 'Climate\\n(Rainfall)',
            fontcolor = black]

  Grass [label = 'Grass biomass\\n(Fuel load)',
          fontcolor = black]

  Fire [label = 'Fire regime\\n(Frequency × Intensity × Season)',
       ]

  Seedling [label = 'Seedling mortality',
            ]

  Topkill [label = 'Juvenile plants\\n& resprouts',
          ]

  EscapeFail [label = 'Demographic\\nbottleneck',
             ]

  Densification [label = 'Bush\\nencroachment',
                 ]

  # Edges with + / – signs
  Climate -> Grass   [label = '(+)']
  Grass   -> Fire    [label = '(+)']

  Fire -> Seedling   [label = '(+)']
  Fire -> Topkill    [label = '(-)']
  Topkill ->  EscapeFail[label = '(+)']

  Seedling -> EscapeFail [label = '(+)']

  EscapeFail -> Densification [label = '(–)']

  Densification -> Grass [label = '(+)']
 
}
")

# Export as PNG
# Convert DiagrammeR object to SVG
fireNC2 <- export_svg(fireC2)

# Write PNG file
rsvg_png(
  charToRaw(fireNC2),
  file = "Plots/2FireConceptual_woody_dynamics.png",
  width = 2000,   # pixels
  height = 2600   # pixels
)




########################################################################
########################################################################

# CONCEPTUAL FRAMEWORK FOR HERBICIDES

grViz("
digraph conceptual_framework {

  graph [layout = dot, rankdir = TB]

  node [shape = rectangle, style = filled, fillcolor = white, color = black, fontname = Helvetica]

  Disturbance [label = 'Disturbance\n(Fire / Cutting)', fillcolor = '#f0f0f0']
  Herbicide [label = 'Aboricide Application\n(Cut-stump)', fillcolor = '#e6f2ff']

  Buds [label = 'Dormant Bud Activation', fillcolor = '#fff5e6']
  NSC [label = 'NSC Mobilisation\n(Roots & Lignotubers)', fillcolor = '#fff5e6']
  Meristems [label = 'Basal Meristem Function', fillcolor = '#fff5e6']

  Resprout [label = 'High Resprouting\nProbability & Vigour', fillcolor = '#fde0dd']
  Mortality [label = 'Meristem Failure\nReduced Resprouting\n↑ Mortality', fillcolor = '#e5f5e0']

  Modulators [label = 'Modulating Factors:\n• Stem size\n• Species traits\n• Timing of application\n• Removal intensity',
              shape = box, fillcolor = '#f7f7f7']

  # Pathways
  Disturbance -> Buds
  Disturbance -> NSC
  Buds -> Resprout
  NSC -> Resprout

  Herbicide -> Meristems
  Meristems -> Mortality

  # Modulators
  Modulators -> Buds [style = dashed]
  Modulators -> NSC [style = dashed]
  Modulators -> Meristems [style = dashed]
}
")

######

# OPTION 2 Herbicides conceptual framework

HerbicideC <- grViz("
digraph Aboricide_Framework {

  graph [layout = dot, rankdir = TB]

  node [shape = box, style = solid, fontname = Helvetica, fontsize = 10]

  # Core nodes
  Disturbance [label = 'Disturbance\n(Thinning)', fillcolor = '#f0f0f0']
  BudActivation [label = 'Dormant bud activation', fillcolor = '#fff5e6']
  NSC [label = 'Non-structural carbohydrates mobilisation', fillcolor = '#fff5e6']
  Resprouting [label = 'High resprouting\nprobability', fillcolor = '#fde0dd']

  Aboricide [label = 'Targeted herbicide application\n(Cut-stump)', fillcolor = '#e6f2ff']
  Uptake [label = 'Systemic uptake &\ndownward translocation', fillcolor = '#e6f2ff']
  MeristemDisruption [label = 'Cambial inhibition\nBasal meristem disruption\nMitotic inhibition,\nCytotoxic effects', fillcolor = '#e6f2ff']
  Mortality [label = 'Reduced resprouting\n↑ stem mortality', fillcolor = '#e5f5e0']

  # Disturbance-only pathway
  Disturbance -> BudActivation [label = '(+)']
  Disturbance -> NSC [label = '(+)']
  BudActivation -> Resprouting [label = '(+)']
  NSC -> Resprouting [label = '(+)']

  # Herbicide mechanistic pathway
  Disturbance -> Aboricide [style = dashed, label = '']
  Aboricide -> Uptake [label = '(+)']
  Uptake -> MeristemDisruption [label = '(+)']
 MeristemDisruption-> Mortality [label = '']

  # NEW: Direct suppressive effect
  Aboricide -> Resprouting [label = '(−)', penwidth = 2]

  # # Modulating influences
  # Modulators -> BudActivation [style = dashed, label = '±']
  # Modulators -> NSC [style = dashed, label = '±']
  # Modulators -> MeristemDisruption [style = dashed, label = '±']

}
")


# export
Herbicides <- export_svg(HerbicideC)

# saving as png
rsvg_png(
  charToRaw(Herbicides),
  file = "Plots/2Herbicides_Conceptual_synthesis_woody_dynamics_icons.png",
  width = 2000,
  height = 2600
)



########### SIMPLIFIED VERSION HERBICIDE APPLICATION

HerbicideSimplified <- grViz("
digraph Herbicide_Framework {

  graph [layout = dot, rankdir = TB]

  node [shape = box, style = solid, fontname = Helvetica]

  # Core nodes
 
  Resprouting [label = 'Resprouting']
  Densification [label = 'Bush encroachment']
  Aboricide [label = 'Targeted herbicide application']
  MeristemDisruption [label = 'Basal meristem disruption']
  Mortalitystem [label = 'Stem mortality']

  # Disturbance-only pathway
  # Disturbance -> BudActivation [label = '(+)']
  # BudActivation -> Resprouting [label = '(+)'] 
  
  # Herbicide mechanistic pathway
  Aboricide -> MeristemDisruption [label = '(-)']
 MeristemDisruption-> Resprouting [label = '(-)']
 Resprouting -> Mortalitystem [label = '(+)']

  # NEW: Direct suppressive effect
  Mortalitystem  -> Resprouting [label = '(-)']
  Resprouting -> Densification [label = '(-)']
  Mortalitystem -> Densification [label = '(-)']
}
")

## export
HerbicidesS <- export_svg(HerbicideSimplified)

# saving as png
rsvg_png(
  charToRaw(HerbicidesS),
  file = "Plots/Herbicides_Conceptual Simplified.png",
  width = 2000,
  height = 2600
)



###################################################################################################

################################# CONCEPTUAL FRAMEWORK FOR GOATS 
############ GOATS OPTION 3 with (+) or (-) to show direction of effect
## (−) = suppresses / reduces / constrains. (+) = promotes / supports / increases


browsersC <- grViz("
digraph browser_framework {

  graph [
    layout = dot,
    rankdir = TB
  ]

  # node [
  #   shape = box,
  #   style = rounded,
  #   fontname = Helvetica
  # ]   ####excluding colours
  
node [shape = rectangle, style = filled, fillcolor = white, color = black, fontname = Helvetica]

  ############################
  # Top-level driver
  ############################
  Browsing [
    label = 'Browsing pressure\n• Defoliation\n•Shoot removal \n• Repeated top-kill',
    style = 'rounded,filled'
  ]

  ############################
  # Immediate demographic effect
  ############################
  SeedlingMortality [
    label = 'Seedling mortality\n• Uprooting / complete defoliation\n• Failure of establishment'
  ]

  ############################
  # Chronic physiological mechanisms
  ############################
  Carbon [
    label = 'Non-structural carbohydrates\n• Loss of photosynthetic tissue\n• Mobilisation of stored C\n• Reduced replenishment'
  ]

  Architecture [
    label = 'Plant architecture\n• Height suppression\n• Multi-stems form'
  ]

  ############################
  # Life stages
  ############################
 

  Saplings [
    label = 'Saplings\n(browse trap)'
  ]

  Resprouts [
    label = 'Resprouts\n(reliance on stored C)'
  ]

  ############################
  # Demographic bottleneck
  ############################
  Bottleneck [
    label = 'Demographic bottleneck\n• Recruitment ↓\n• Growth rates ↓\n• Size escape delayed'
  ]

  Outcome [
    label = '\n Suppress woody encroachment',
    style = 'rounded,filled' #####,fillcolor = palegreen  # colouring the cell/node
    
  ]

  ############################
  # Signed edges
  ############################

  # Direct lethal browsing
  Browsing -> SeedlingMortality [
    label = '(+)',
    fontsize = 14
  ]

  SeedlingMortality -> Bottleneck [
    label = '(+)',
    fontsize = 14
  ]

  # Browsing effects on physiology
  Browsing -> Carbon [
    label = '(-)',
    fontsize = 14
  ]

  Browsing -> Architecture [
    label = '(-)',
    fontsize = 14
  ]

  # Carbon effects (no direct link to seedlings)
  Carbon -> Saplings [
    label = '(+)',
    fontsize = 14
  ]

  Carbon -> Resprouts [
    label = '(+)',
    fontsize = 14
  ]

  # Architectural effects
  Architecture -> Saplings [
    label = '(+)',
    fontsize = 14
  ]

  Architecture -> Resprouts [
    label = '(+)',
    fontsize = 14
  ]

  # Life stages to bottleneck
  Saplings -> Bottleneck [
    label = '(-)',
    fontsize = 14
  ]

  Resprouts -> Bottleneck [
    label = '(+)',
    fontsize = 14
  ]

  # Bottleneck to system outcome
  Bottleneck -> Outcome [
    label = '',
    fontsize = 14
  ]

  ############################
  # Ranking
  ############################
  { rank = same; Carbon; Architecture }
  { rank = same;  Saplings; Resprouts }
}
")


# export
svgBImages <- export_svg(browsersC)

# saving as png
rsvg_png(
  charToRaw(svgBImages),
  file = "Plots/Browsers_Conceptual_synthesis_woody_dynamics_icons.png",
  width = 2000,
  height = 2600
)

#############################################
#####  BROWSERS CONCEPTUAL FRAMEWORK - REFINED TO USE NEUTRAL WORDS


BrowsersR <- grViz("
digraph browser_framework {

  graph [
    layout = dot,
    rankdir = TB
  ]

  # node [
  #   shape = box,
  #   style = rounded,
  #   fontname = Helvetica
  # ]   ####excluding colours
  
node [shape = rectangle, style = filled, fillcolor = white, color = black, fontname = Helvetica]

  ############################
  # Top-level driver
  ############################
  Browsing [
    label = 'Browsing pressure',
    style = 'rounded,filled'
  ]

  ############################
  # Immediate demographic effect
  ############################
  SeedlingMortality [
    label = 'Seedlings']

  ############################
  # Chronic physiological mechanisms
  ############################
  Carbon [
    label = 'Non-structural carbohydrates'
  ]

  Architecture [
    label = 'Plant architecture\n• (Height\n• Multi-stems)'
  ]

  ############################
  # Life stages
  ############################
 

  Saplings [
    label = 'Saplings\n(browse trap)'
  ]

  Resprouts [
    label = 'Resprouts'
  ]

  ############################
  # Demographic bottleneck
  ############################
  Bottleneck [
    label = 'Demographic bottleneck']

  Outcome [
    label = '\n Bush encroachment',
    style = 'rounded,filled' #####,fillcolor = palegreen  # colouring the cell/node
  ]

  ############################
  # Signed edges
  ############################

  # Direct lethal browsing
  Browsing -> SeedlingMortality [
    label = '(-)',
    fontsize = 14
  ]

  SeedlingMortality -> Bottleneck [
    label = '(+)',
    fontsize = 14
  ]

  # Browsing effects on physiology
  Browsing -> Carbon [
    label = '(-)',
    fontsize = 14
  ]

  Browsing -> Architecture [
    label = '(-)',
    fontsize = 14
  ]

  # Carbon effects (no direct link to seedlings)
  Carbon -> Saplings [
    label = '(-)',
    fontsize = 14
  ]

  Carbon -> Resprouts [
    label = '(-)',
    fontsize = 14
  ]

  # Architectural effects
  Architecture -> Saplings [
    label = '(+)',
    fontsize = 14
  ]

  

  # Life stages to bottleneck
  Saplings -> Bottleneck [
    label = '(+)',
    fontsize = 14
  ]

  Resprouts -> Bottleneck [
    label = '(+)',
    fontsize = 14
  ]

  # Bottleneck to system outcome
  Bottleneck -> Outcome [
    label = '(-)',
    fontsize = 14
  ]

  ############################
  # Ranking
  ############################
  { rank = same; Carbon; Architecture }
  { rank = same;  Saplings; Resprouts }
}
")

# Export as png
# export
BrowserRefined <- export_svg(BrowsersR)

# saving as png
rsvg_png(
  charToRaw(BrowserRefined),
  file = "Plots/2Browsers_Conceptual_synthesis.png",
  width = 2000,
  height = 2600
)



###################### CONCEPTUAL FRAMEWORK THINNING
##  Thinning conceptual framework

thinning2<-grViz("
digraph cutting_framework {

  # Graph attributes
  graph [layout = dot, rankdir = LR]

  # Nodes 
  node [shape = box, style = solid, fontname = Helvetica, fontsize = 10]
  Cutting [label='Thinning', shape=box, style=filled, fillcolor=white]
  Adult_Canopy [label='Woody Cover', shape=ellipse, style=filled, fillcolor=white]
  Juveniles [label='Juvenile plants', shape=ellipse, style=filled, fillcolor=white]
  Grass [label='Grass Competition', shape=ellipse, style=filled, fillcolor=white]
  Reduced_Densification [label='Bush encroachment', shape=ellipse, style=filled, fillcolor=white]

  # Edges: main causal flow
  Cutting -> Adult_Canopy [label='(−)', penwidth=1.0]
  Adult_Canopy -> Juveniles [label='(-)', penwidth=1.0]
  Juveniles -> Reduced_Densification [label='(-)', penwidth=1.0]

  # Edges: direct effects of cutting on juveniles
  Cutting -> Juveniles [label='(−)', penwidth=1.0]

  # Edges: indirect effects via grass
  Cutting -> Grass [label='(+)', penwidth=1.0]
  Grass -> Juveniles [label='(−)', penwidth=1.0]

}
")


# export
thinning2 <- export_svg(thinning2)

# saving as png
rsvg_png(
  charToRaw(thinning2),
  file = "Plots/2Thinning_Conceptual_woody_dynamics_icons.png",
  width = 2000,
  height = 2600
)



###########################################################################################
###########################################################################################


# CONCEPTUAL DIAGRAM for BROWSERS ON GRASS RICHNESS & DIVERSITY 


browsers_grass <- grViz("
digraph BrowserGrassDiversity {

  #### graph [layout = dot, rankdir = LR] # for horizontal layout 
  
  graph [layout = dot, rankdir = TB]  ## for vertical layout


node [shape = rectangle, style = filled, fillcolor = white, color = black, fontname = Helvetica]

  # --- Driver ---
  Browsers [label = 'Browsers']

  # --- Primary Ecological Effects ---
  WoodySuppression [label = 'Woody plant suppression']
  
  # --- Resource & Structural Pathways ---
  ResourceRelease [label = 'Increased light & soil moisture\\navailability']
  ReducedCompetition [label = 'Reduced tree–grass competition']
  Recruitment [label = 'Enhanced recruitment\\nof subordinate species']

  # --- Community Processes ---
  PerennialPersistence [label = 'Increased grass\\ncompetition']
  CompetitiveBalance [label = 'Reduced dominance\\nby few species']

  # --- Outcomes ---
  SpeciesRichness [label = 'Grass species richness']
  FunctionalDiversity [label = 'Grass diversity']

 # --- Signed Connections ---
  Browsers -> WoodySuppression [label = '(+)']
  WoodySuppression -> ResourceRelease [label = '(+)']
  WoodySuppression -> ReducedCompetition [label = '(+)']
  ResourceRelease -> PerennialPersistence [label = '(+)']
  ReducedCompetition -> PerennialPersistence [label = '(+)']
   ReducedCompetition -> Recruitment [label = '(+)']

  

  Recruitment -> CompetitiveBalance [label = '(+)']
  PerennialPersistence -> CompetitiveBalance [label = '(+)']

  CompetitiveBalance -> SpeciesRichness [label = '(+)']
  SpeciesRichness -> FunctionalDiversity [label = '(+)']

  # Explicit negative control of woody plants
  ##Browsers -> WoodySuppression [label = '(+)']
}
")

# export
browgrass <- export_svg(browsers_grass)

# saving as png
rsvg_png(
  charToRaw(browgrass),
  file = "Plots/Browsersgrass_Conceptual_framework_icons.png",
  width = 2000,
  height = 2600
)


################################### BROWSERS ON GRASS DIVERSITY - NEUTRAL WORDS


BrowesrsN <- grViz("
digraph BrowserGrassDiversity {

  #### graph [layout = dot, rankdir = LR] # for horizontal layout 
  
  graph [layout = dot, rankdir = TB]  ## for vertical layout


node [shape = rectangle, style = filled, fillcolor = white, color = black, fontname = Helvetica]

  # --- Driver ---
  Browsers [label = 'Browsers']

  # --- Primary Ecological Effects ---
  WoodySuppression [label = 'Woody plant encroachment']
  
  # --- Resource & Structural Pathways ---
  ResourceRelease [label = 'Light & soil moisture']
  ReducedCompetition [label = 'Tree–grass competition']
  Recruitment [label = 'Recruitment\\nof subordinate species']

  # --- Community Processes ---
  PerennialPersistence [label = 'Grass\\nbiomass']
  CompetitiveBalance [label = 'Dominance\\nby few species']

  # --- Outcomes ---
  SpeciesRichness [label = 'Grass species richness']
  FunctionalDiversity [label = 'Grass diversity']

 # --- Signed Connections ---
  Browsers -> WoodySuppression [label = '(-)']
  WoodySuppression -> ResourceRelease [label = '(+)']
  WoodySuppression -> ReducedCompetition [label = '(-)']
  ResourceRelease -> PerennialPersistence [label = '(+)']
  ReducedCompetition -> PerennialPersistence [label = '(+)']
   ReducedCompetition -> Recruitment [label = '(+)']


  Recruitment -> CompetitiveBalance [label = '(-)']
  PerennialPersistence -> CompetitiveBalance [label = '(-)']

  CompetitiveBalance -> SpeciesRichness [label = '(+)']
  SpeciesRichness -> FunctionalDiversity [label = '(+)']

  # Explicit negative control of woody plants
  ##Browsers -> WoodySuppression [label = '(-)']
}
")

## export
browgrass2 <- export_svg(BrowesrsN)

# saving as png
rsvg_png(
  charToRaw(browgrass2),
  file = "Plots/Browsersgrass_Conceptual_framework_icons.png",
  width = 2000,
  height = 2600
)


########################################################################################
#################################################################################################

# CONCEPTUAL FRAMEWORK FOR THINNING FIRE AND BROWSERS


TFB_conceptual <- grViz("
digraph Woody_Suppression_Framework {

  graph [layout = dot, rankdir = TB]

  node [shape = box, style = filled, fillcolor = white, fontname = Helvetica]

  # =========================
  # TOP-LEVEL DRIVERS
  # =========================
  Cutting     [label = 'Woody plant thinning']
  Fire        [label = 'Fire']
  Browsing    [label = 'Browsers']

  # =========================
  # DEMOGRAPHIC STAGES
  # =========================
  Adults      [label = 'Mature trees']
  Seedlings   [label = 'Seedlings']
  Saplings    [label = 'Saplings']
  Resprouts   [label = 'Resprouts']

  # =========================
  # JUVENILE MECHANISMS
  # =========================
  JCarbon     [label = 'Non-structural\ncarbohydrates depletion']
  Bottleneck  [label = 'Demographic\nBottleneck', shape = diamond]
  Grass       [label = 'Grass Dominance']

  # =========================
  # FINAL OUTCOME
  # =========================
  Densification [label = 'Woody plant\nencroachment', shape = oval]

  # =========================
  # DIRECT DISTURBANCE EFFECTS
  # =========================

  Cutting  -> Adults     [label = '(-)']
  Cutting  -> Resprouts  [label = '(+)']

  Fire     -> Seedlings  [label = '(-)']
  Fire     -> Saplings   [label = '(-)']
  Fire     -> Resprouts  [label = '(-)']

  Browsing -> Seedlings  [label = '(-)']
  Browsing -> Saplings   [label = '(-)']
  Browsing -> Resprouts  [label = '(-)']

  # =========================
  # EXPLICIT CARBON DEPLETION
  # =========================

  Saplings  -> JCarbon   [label = '(-)']
  Resprouts -> JCarbon   [label = '(-)']
  
 

  # =========================
  # SIZE TRANSITIONS
  # =========================

  Seedlings -> Saplings     [label = '(+)']
 

  # =========================
  # GRASS FEEDBACK
  # =========================

  Browsing -> Grass     [label = '(+)']
  Grass    -> Seedlings [label = '(-)']
  Grass    -> Fire      [label = '(+)']

  # =========================
  # BOTTLENECK STRUCTURE
  # =========================

  Seedlings -> Bottleneck [label = '(+)']
  Saplings  -> Bottleneck [label = '(+)']
  Resprouts -> Bottleneck [label = '(+)']
  JCarbon   -> Bottleneck [label = '(+)']


  Bottleneck -> Adults   [label = '(-)']

  # =========================
  # FINAL DENSIFICATION PATHWAY
  # =========================

  Adults -> Densification [label = '']

}
")



# export
tfb <- export_svg(TFB_conceptual)

# saving as png
rsvg_png(
  charToRaw(tfb),
  file = "Plots/TFB_Conceptual_framework_icons.png",
  width = 2000,
  height = 2600
)



################### option 2 simplified version of TFB conceptual framework

grViz("
digraph HighImpact_Woody_Framework {

  graph [layout = dot, rankdir = TB]

  node [shape = box, style = filled, fillcolor = white, fontname = Helvetica]

  # Top-level drivers
  Cutting   [label = 'Mechanical Cutting']
  Fire      [label = 'Fire']
  Browsing  [label = 'Meso-browsers']

  # Key processes
  SeedlingMort [label = 'Seedling Mortality']
  CarbonHeight [label = 'Carbon Depletion & Height Suppression']
  GrassFire    [label = 'Grass-Fire Feedback']

  # Outcome
  Suppression  [label = 'Suppressed Woody Densification', shape = oval, fillcolor = palegreen]

  # ======================
  # Direct pathways
  # ======================
  Cutting  -> CarbonHeight [label = '(−)']
  Fire     -> SeedlingMort [label = '(−)']
  Browsing -> SeedlingMort [label = '(−)']
  Browsing -> CarbonHeight [label = '(−)']

  # Indirect pathway
  Browsing -> GrassFire [label = '(+)']
  GrassFire -> SeedlingMort [label = '(−)']

  # Converging to outcome
  SeedlingMort -> Suppression [label = '(−)']
  CarbonHeight -> Suppression [label = '(−)']

}
")


############################################################

##  THINNING, HERBICIDE, FIRE, BROWSERS CONCEPTUAL FRAMEWORK


THFB <- grViz("
digraph Woody_Suppression_Framework {

  graph [layout = dot, rankdir = TB]

  node [shape = box, style = filled, fillcolor = white, fontname = Helvetica]

  # =========================
  # TOP-LEVEL DRIVERS
  # =========================
  Cutting     [label = 'Woody plant thinning']
  Fire        [label = 'Fire']
  Browsing    [label = 'Browsers']
  Herbicide   [label = 'Targeted herbicide application']  

  # =========================
  # DEMOGRAPHIC STAGES
  # =========================
  Adults      [label = 'Mature trees']
  Seedlings   [label = 'Seedlings']
  Saplings    [label = 'Saplings']
  Resprouts   [label = 'Resprouts']

  # =========================
  # JUVENILE MECHANISMS
  # =========================
  JCarbon     [label = 'Non-structural\ncarbohydrates depletion']
  Bottleneck  [label = 'Demographic\nBottleneck', shape = diamond]
  Grass       [label = 'Grass competition']

  # =========================
  # FINAL OUTCOME
  # =========================
  Densification [label = 'Suppress bush\nencroachment', shape = oval]

  # =========================
  # DIRECT DISTURBANCE EFFECTS
  # =========================

  Cutting  -> Adults     [label = '(-)']
  Cutting  -> Resprouts  [label = '(+)']

  Herbicide -> Resprouts [label = '(-)']  # Herbicides reduce resprouts

  Fire     -> Seedlings  [label = '(-)']
  Fire     -> Saplings   [label = '(-)']
  Fire     -> Resprouts  [label = '(-)']

  Browsing -> Seedlings  [label = '(-)']
  Browsing -> Saplings   [label = '(-)']
  Browsing -> Resprouts  [label = '(-)']

  # =========================
  # EXPLICIT CARBON DEPLETION
  # =========================

  Saplings  -> JCarbon   [label = '(-)']
  Resprouts -> JCarbon   [label = '(-)']
  
 

  # =========================
  # SIZE TRANSITIONS
  # =========================

  Seedlings -> Saplings     [label = '(+)']
 

  # =========================
  # GRASS FEEDBACK
  # =========================

  Fire -> Grass         [label = '(+)']
  Grass    -> Seedlings [label = '(-)']
  Grass    -> Fire      [label = '(+)']
  Cutting -> Grass      [label = '(+)']
  
  # =========================
  # BOTTLENECK STRUCTURE
  # =========================

  Seedlings -> Bottleneck [label = '(+)']
  Saplings  -> Bottleneck [label = '(+)']
  Resprouts -> Bottleneck [label = '(+)']
  JCarbon   -> Bottleneck [label = '(+)']
  Herbicide -> Bottleneck [label = '(+)']  

  Bottleneck -> Adults   [label = '(-)']

  # =========================
  # FINAL DENSIFICATION PATHWAY
  # =========================

  Adults -> Densification [label = '']

}
")


# export
thfb <- export_svg(THFB)

# saving as png
rsvg_png(
  charToRaw(thfb),
  file = "Plots/THFB_Conceptual_framework_icons.png",
  width = 2000,
  height = 2600
)




###########################################################################################
##########################################################################################


# all in one Conceptual framework THFB&Grasses

Combined <-  grViz("
digraph Woody_Suppression_Framework {

  graph [layout = dot, rankdir = TB]

  node [shape = box, style = filled, fillcolor = white, fontname = Helvetica]

  # =========================
  # TOP-LEVEL DRIVERS
  # =========================
  Cutting     [label = <<B>WOODY PLANT THINNING 🪵🪵</B>>] 
  Fire        [label = <<B>FIRE 🔥🔥</B>>]
  Browsing    [label = <<B>BROWSERS 🐐🐐</B>>]
  Herbicide   [label = <<B>TARGETED HERBICIDE APPLICATION 🧴</B>>] 

  # =========================
  # DEMOGRAPHIC STAGES
  # =========================
  Adults      [label = 'Mature trees']
  Seedlings   [label = 'Seedlings']
  Saplings    [label = 'Saplings']
  Resprouts   [label = 'Resprouts']

  # =========================
  # JUVENILE MECHANISMS
  # =========================
  JCarbon     [label = 'Non-structural\ncarbohydrates depletion']
  Bottleneck  [label = 'Demographic\nBottleneck', shape = diamond]
  Grass       [label = 'Grass competition']
  GrassR      [label = 'Grass species richness']
  GrassD      [label = 'Grass diversity']
  # =========================
  # FINAL OUTCOME
  # =========================
  Densification [label = 'Suppress bush\nencroachment', shape = oval]

  # =========================
  # DIRECT DISTURBANCE EFFECTS
  # =========================

  Cutting  -> Adults     [label = '(-)']
  Cutting  -> Resprouts  [label = '(+)']

  Herbicide -> Resprouts [label = '(-)']  # Herbicides reduce resprouts
  Resprouts -> Grass [headlabel = '(+)', labeldistance = 3.0]
  
  Fire     -> Seedlings  [label = '(-)']
  Fire     -> Saplings   [label = '(-)']
  Fire     -> Resprouts  [label = '(-)']

  Browsing -> Seedlings  [label = '(-)']
  Browsing -> Saplings   [label = '(-)']
  Browsing -> Resprouts  [label = '(-)']

  # =========================
  # EXPLICIT CARBON DEPLETION
  # =========================

  Saplings  -> JCarbon   [label = '(-)']
  Resprouts -> JCarbon   [label = '(-)']
  
 

  # =========================
  # SIZE TRANSITIONS
  # =========================

  Seedlings -> Saplings     [label = '(+)']
 

  # =========================
  # GRASS FEEDBACK
  # =========================

  Fire -> Grass         [label = '(+)']
  Fire -> GrassR    [taillabel = '(+)', labeldistance = 6.5] #to move sign
  Grass    -> Seedlings [headlabel = '(-)', labeldistance = 3.0]
  Grass    -> Fire      [label = '(+)']
  Cutting -> Grass      [label = '(+)']
  Densification -> GrassR [label = '(+)']
  GrassR -> GrassD [label = '(+)']
  Browsing ->Densification [label = '(-)']
  Densification -> Grass [label = '(+)']
  
  # =========================
  # BOTTLENECK STRUCTURE
  # =========================

  Seedlings -> Bottleneck [label = '(+)']
  Saplings  -> Bottleneck [label = '(+)']
  Resprouts -> Bottleneck [label = '(+)']
  JCarbon   -> Bottleneck [label = '(+)']
  Herbicide -> Bottleneck [label = '(+)']  

  Bottleneck -> Adults   [label = '(-)']

  # =========================
  # FINAL DENSIFICATION PATHWAY
  # =========================

  Adults -> Densification [label = '']

}
")

# export
AllTg <- export_svg(Combined)

# saving as png
rsvg_png(
  charToRaw(AllTg),
  file = "Plots/Integrated_Conceptual_framework_icons.png",
  width = 2000,
  height = 2600
)


##########################################################################
###########################################################################

# Drivers of bush encroachment


driversBE <- grViz("
digraph bush_encroachment {

  graph [layout = dot, rankdir = TB]

    node [shape = box, style = filled, fillcolor = white, fontname = Helvetica]

  # ---- DRIVER LAYER ----
  Nfert   [label = 'Nitrogen fertilisation']
  CO2     [label = 'Elevated CO₂']
  Rain    [label = 'Rainfall']
  Soil    [label = 'Soil properties\n(texture, nutrients)', fillcolor = '#1B5E20', fontcolor = white]
  Grazing [label = 'Grazing pressure']
  Browsers[label = 'Browsing pressure']
  Fire    [label = 'Fire']

  # ---- PROCESS LAYER ----
  Grass   [label = 'Grass biomass']
  Recruit [label = 'Woody plant recruitment']
  #Growth  [label = 'Demographic recruitment']

  # ---- OUTCOME ----
  Encroach [label = 'Bush encroachment' shape = oval]

  # ---- LINKS ----
  Grazing -> Grass [label = '(-)']
  Grass -> Recruit [label = '(+)']
  Grass -> Fire [label = '(-)']

  Browsers -> Recruit [label = '(+)']
  Fire -> Recruit [label = '(+)']

  Rain -> Recruit [label = '(+)']
  Soil -> Recruit [label = '(+)']
  #Soil -> Growth [label = '(+)']

  Nfert -> Recruit [label = '(+)']
  CO2 -> Recruit [label = '(+)']

  Recruit -> Encroach [label = '(+)']
  #Growth -> Encroach[label = '(+)']
}
")


## export
dbe <- export_svg(driversBE)

# saving as png
rsvg_png(
  charToRaw(dbe),
  file = "Plots/DriversBE_Conceptual_framework.png",
  width = 2000,
  height = 2600
)
