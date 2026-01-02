### THIS SCRIPT HAS BEEN USED TO CREATE A MULTI-PANEL FIGURE FOR EACH RESPONSE VARIABLE 
  # THAT SHOWS COLOR CODED VIOLIN PLOTS and Marginal effects. 


## increasing font size for x and y axis
SeedViolin<- ggplot(strt_comparison2, 
                 aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +    
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  labs(x = "Treatment", 
       y = "Seedlings density per ha",
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )+
 scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


## violin plot seedling delta
Seedbviolin <- ggplot(Seedlings_Delta1,
                   aes(x = Treatment, y = delta_Seeddens, fill = Fencing))+ 
  geom_violin(trim = FALSE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", 
       y = "Change in Seedlings density per ha",
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
    axis.text = element_text(size = 6)) +
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))


##marginal effects plot
Seeden1 <- ggpredict(Seedl5, terms = c("Treatment", "Fencing"))
See <- plot(Seeden1, colors = c( "#d8b365", "#8c510a"))  +    
  labs(y = "Change in Seedlings density per ha",
       x = "Treatment") +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 6),      # Axis titles
    axis.text = element_text(size = 6))


## saving marginal effects plot
 #ggsave(See,filename ="Plots/ME Change in seedling density.png",
       width = 16, height = 14, units = "cm")  


########### USING EMMEANS
# Estimated marginal means for Treatment within Kraaling (if needed)
Seedemm <- emmeans(Seedl5, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
contrast_vs_control <- contrast(Seedemm, method = "trt.vs.ctrl", ref = "C")
summary(contrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Seedcld_emm <- cld(Seedemm, adjust = "tukey", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Seedcld_emm)


# prepare clean database for plotting
plot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot
semm <- ggplot(plot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = "Change in seedling density per ha",
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))



## Combine the plots in a single layout
multi_panelsE <- (SeedViolin/Seedbviolin/ semm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 6, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 7),        # Increase axis label font size
    axis.title = element_text(size = 7),       # Increase axis title font size
    plot.tag = element_text(size = 7, hjust = 0)  # Ensure left alignment
  )

#saving using ggsave
 ggsave(multi_panelsE,filename ="Plots/Multipanel EMMViolin Seedlings.png",
       width = 16, height = 14, units = "cm")  




################### Violin SAPLINGS density
SapViolin<- ggplot(sptrt_comparison2, 
                    aes(x = Treatment, y = density_ha, fill = Period)) + facet_wrap(~Fencing)+
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") + 
  labs(x = "Treatment", 
       y = "Sapling density per ha",
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 8), # Axis titles size reduced to 8 from 12
    axis.text = element_text(size = 8))+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


#Sapling density  Delta - simple Mean points on the violin
SpViolin <- ggplot(Saplings_Delta,aes(x = Treatment, y = delta_Sapsdens,fill = Fencing))+
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Sapling density per ha") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  #scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))



### Marginal effects ggpredict for sapling density

Sapden1 <- ggpredict(Sapl5, terms = c("Treatment", "Fencing"))
Sap <- plot(Sapden1,colors = c( "#d8b365", "#8c510a"))  +    
  labs(y = "Change in Sapling density per ha",
       x = "Treatment") +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8))




###
##### EMM FOR SAPLING DENSITY
# Estimated marginal means for Treatment within Fencing 
Saplemm <- emmeans(Sapl5, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
spcontrast_vs_control <- contrast(Saplemm, method = "trt.vs.ctrl", ref = "C")
summary(spcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Saplcld_emm <- cld(Saplemm, adjust = "tukey", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Saplcld_emm)


# prepare clean database for plotting
splot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot for saplings
spemm <- ggplot(splot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 1.5 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = "Change in sapling density per ha",
  ) +
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))




# Combine the plots in a single layout - SAPLINGS
multi_panel <- (SapViolin / SpViolin/ spemm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1,1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 8),        # Increase axis label font size
    axis.title = element_text(size = 8),       # Increase axis title font size
    plot.tag = element_text(size = 8, hjust = 0)  # Ensure left alignment
  )

#saving using ggsave
ggsave(multi_panel,filename ="Plots/Multipanel EMMViolin Saplings.png",
       width = 16, height = 14, units = "cm")  




####### RESPROUTS

##violin plot
RespVio <- ggplot(resprouts_df,
                  aes(x = Treatment, y = No_of_resprouts)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Average no.of resprouts per cut stump") +
  theme_beautiful() +
  theme(
    axis.title = element_text(size = 14),      # Axis titles
    axis.text = element_text(size = 12)        # Axis tick labels
  )



##saving Violin PLOT - resprouts
ggsave(RespVio,filename ="Plots/RESPROUTS VIOLIN plot.png",
       width = 16, height = 14, units = "cm")  



#########################

#### Grass HEIGHT
Ghviolin<- ggplot(grass_height, 
               aes(x = Treatment, y = mean_DPM_Height, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") + 
  labs(x = "Treatment", 
       y = "Grass height (cm)",
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 9),      # Axis titles
    axis.text = element_text(size = 9))+
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


###Visualise: Violin plot of Δ‑grass height. FONT INCREASED
GrasVio <- ggplot(delta_GRHeight,
                  aes(x = Treatment, y = delta_GR, fill = Fencing))+ 
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Grass height (cm)") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  #scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))



##Marginal effects grass height

Gheight <- ggpredict(Grashg1, terms = c("Treatment", "Fencing"))
ght1 <-plot(Gheight,colors = c( "#d8b365", "#8c510a")) + 
  labs(y = "Change in Grass height (cm)",
       x = "Treatment") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()+ 
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )


# Combine the plots in a single layout - grass height
multi_panelR <- (Ghviolin / GrasVio/ ght1) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 8),        # Increase axis label font size
    axis.title = element_text(size = 8),       # Increase axis title font size
    plot.tag = element_text(size = 8, hjust = 0)  # Ensure left alignment
  )

##ggsave multipanel grass richness
ggsave(multi_panelR,filename ="Plots/Multipanel OMViolin Grass height.png",
       width = 16, height = 14, units = "cm")  



#############Grass species richness violin 

GrRViolin <- ggplot(Grass_rich, 
            aes(x = Treatment, y = spp_richness, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +   
  labs(x = "Treatment", 
       y = "Grass species richness") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  ) +
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))



#Grass species richness Delta - simple Mean points on the violin
Grspp <- ggplot(grassR_delta,aes(x = Treatment, y = delta, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in grass species richness") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  #scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown
  scale_fill_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365"))



## Marginal effects for grass species richness
SppR2 <- ggpredict(modelRich, terms = c("Treatment", "Fencing"))
grsp1 <-plot(SppR2, colors = c( "#d8b365", "#8c510a")) + 
  labs(y = "Change in grass species richness",
       x = "Treatment") +
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle(NULL)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )


######## EMM FOR GRASS RICHNESS
# Estimated marginal means for Treatment within Fencing 
GRemm <- emmeans(modelRich, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
grcontrast_vs_control <- contrast(Saplemm, method = "trt.vs.ctrl", ref = "C")
summary(grcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
Grlcld_emm <- cld(GRemm, adjust = "tukey", Letters = letters, type = "response")
cld_tbl <- as.data.frame(Grlcld_emm)


# prepare clean database for plotting
gplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot for grass richness
Grlemm <- ggplot(gplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.25 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  theme_classic(base_size = 14) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = "Change in grass species richness",
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))



# Combine the plots in a single layout species richness
multi_panelR <- (GrRViolin / Grspp/ Grlemm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1, 1, 1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 8, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 8),        # Increase axis label font size
    axis.title = element_text(size = 8),       # Increase axis title font size
    plot.tag = element_text(size = 8, hjust = 0)  # Ensure left alignment
  )

##ggsave multipanel grass richness
ggsave(multi_panelR,filename ="Plots/Multipanel EMMViolin Grass richness.png",
       width = 16, height = 14, units = "cm")  




########## VIOLIN GRASS DIVERSITY
GSWViolin <- ggplot(grSWdiversity,
               aes(x = Treatment, y =Shannon_Diversity, fill = Period)) + facet_wrap(~Fencing)+ 
  geom_violin(trim = TRUE)+
  geom_hline(yintercept = 0, linetype = "dashed") +  
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 1, color = "black") +
  labs(x = "Treatment", 
       y = "Shannon-Weiner diversity",
  ) + theme_classic()+
  theme(axis.title = element_text(size = 8),
        axis.text = element_text(size = 8)) +  # Axis tick labels
  scale_fill_manual(values = c("Pre-treatment" = "#1b7837", "Post-treatment" = "#a6dba0"))


#### Grass Diversity Mean points on the violin
GrSW <- ggplot(SW_Delta,aes(x = Treatment, y =delta_SW, fill = Fencing)) +
  geom_violin(alpha = 0.7, position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               position = position_dodge(0.8), 
               size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Treatment", y = "Change in Shannon-Weiner diversity") +
  theme_classic() +
  #scale_fill_manual(values = c("Fenced" = "saddlebrown", "Unfenced" = "navajowhite"))
  #scale_fill_brewer(palette = "YlOrBr")  # Yellow-Orange-Brown palette
  scale_fill_manual(values = c("#8c510a", "#d8b365"))  # Dark brown, light brown


##Marginal effects grass diversity
preds <- ggpredict(GrasD, terms = c("Treatment", "Fencing"))
grdiv1 <- plot(preds,colors = c( "#d8b365", "#8c510a")) +  
  labs(y = "Change in Shannon-Weiner diversity",
       x = "Treatment") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 8),      # Axis titles
    axis.text = element_text(size = 8)        # Axis tick labels
  )



######
## ### EMM FOR GRASS DIVERSITY
# Estimated marginal means for Treatment within Fencing 
GDemm <- emmeans(modelRich, ~ Treatment | Fencing, type = "response")

# Compare each treatment to Control with Tukey adjustment (or "none" if you only want vs control)
gDcontrast_vs_control <- contrast(GDemm, method = "trt.vs.ctrl", ref = "C")
summary(gDcontrast_vs_control, infer = TRUE)

# generate letters using cld in multicomp package
GDcld_emm <- cld(GDemm, adjust = "tukey", Letters = letters, type = "response")
cld_tbl <- as.data.frame(GDcld_emm)  # TUKEY  hanged to Sidak


# prepare clean database for plotting
gdplot_df <- cld_tbl %>%
  rename(
    EMM = emmean,
    CI_lower = lower.CL,
    CI_upper = upper.CL,
    Group = .group
  ) %>%
  mutate(Group = str_trim(Group))  # Clean whitespace



## Visualisation using ggplot for grass diversity
Gdm <- ggplot(gdplot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                position = position_dodge(width = 0.35), width = 0.12) +
  geom_text(aes(label = Group,
                y = CI_upper + 0.25 * max(EMM)),
            position = position_dodge(width = 0.35), size = 3, color = "black") +
  scale_color_manual(values = c("Fenced" = "#8c510a", "Unfenced" = "#d8b365")) +
  theme_classic(base_size = 14) +
  labs(color = "Fencing")+  # Optional: rename legend title
  labs(
    x = "Treatment",
    y = "Change in Shannon-Weiner index",
  ) +
  theme_classic()+ 
  ggtitle(NULL)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
  theme(
    axis.title = element_text(size = 12),      # Axis titles
    axis.text = element_text(size = 12))



#### Combine the plots in a single layout
multi_panelSWd <- (GSWViolin / GrSW/ Gdm) +   # "/" for stacking vertically, or "|" for side-by-side
  plot_layout(heights = c(1,1,  1)) +  # Adjust relative heights
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')',
    theme = theme(plot.tag = element_text(size = 7.5, hjust = 0))  # Left align tags
  ) &
  theme(
    axis.text = element_text(size = 7.5),        # Increase axis label font size
    axis.title = element_text(size = 7.5),       # Increase axis title font size
    plot.tag = element_text(size = 7.5, hjust = 0)  # Ensure left alignment
  )


##ggsave multipanel grass richness
ggsave(multi_panelSWd,filename ="Plots/Multipanel EMMViolin Grass diversity.png",
       width = 16, height = 14, units = "cm")  




#####################################################################
