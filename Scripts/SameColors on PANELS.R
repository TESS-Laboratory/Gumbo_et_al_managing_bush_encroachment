
#### plot with PERIOD AS facet :  SEEDLING DENSITY
SeedViolin2 <- ggplot(strt_comparison2, 
       aes(x = Treatment, y = density_ha, fill = Fencing)) + 
  facet_wrap(~Period, nrow = 1) +
  geom_violin(trim = TRUE,position = position_dodge(0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean,
               geom = "point",
               position = position_dodge(0.8),
               size = 1.4,
               color = "black") +
  labs(
    x = "Treatment",
    y = expression("Seedling density" * ha^{-1}),
    fill = "Fencing"
  ) + theme_classic() + theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "right"
  ) +
  
  scale_fill_manual(
    values = c(
      "Unfenced" = "purple",
      "Fenced"   = "green"
    )
  )
  

### grass BIOMASS

ggplot(biomass2, 
       aes(x = Treatment, y = mean_Biomass, fill = Fencing))+ facet_wrap(~Period, nrow = 1)+ 
  geom_violin(trim = TRUE,position = position_dodge(0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = mean,
               geom = "point",
               position = position_dodge(0.8),
               size = 1.4,
               color = "black")  + 
  labs(x = "Treatment", 
       #y = "Above-ground grass biomass (kgDM/ha)",
       y = expression("Above-ground grass biomass ("*kg~ha^{-1}*")")
  ) + 
  theme_classic()+ theme(
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 9, face = "bold"),
    legend.position = "right"
  ) +
  scale_fill_manual(
    values = c(
      "Unfenced" = "purple",
      "Fenced"   = "green"
    )
  )

  
#### 
  
  ## violin plot seedling delta
  Seedbviolin2 <- ggplot(Seedlings_Delta1,
                        aes(x = Treatment, y = delta_Seeddens, fill = Fencing))+ 
    geom_violin(trim = FALSE)+
    geom_hline(yintercept = 0, linetype = "dashed") +  
    stat_summary(fun = mean, geom = "point", 
                 position = position_dodge(0.8), 
                 size = 1, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Treatment", 
         #y = "Change in Seedlings density per ha",
         y = expression("Change in Seedling density "*ha^{-1}*"")
    ) +
    theme_classic() +
    theme(
      axis.title = element_text(size = 6),  # Axis titles reduced from 12 to 8
      axis.text = element_text(size = 6)) +
    scale_fill_manual(values = c("Fenced"   = "green", "Unfenced" = "purple"))
  
  
  ##
  ## Visualisation using ggplot
  semm2 <- ggplot(plot_df, aes(Treatment, EMM, color = Fencing, group = Fencing)) +
    geom_point(position = position_dodge(width = 0.35), size = 2.5) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                  position = position_dodge(width = 0.35), width = 0.12) +
    geom_text(aes(label = Group,
                  y = CI_upper + 0.5 * max(EMM)),
              position = position_dodge(width = 0.35), size = 3, color = "black") +
    scale_color_manual(values = c("Fenced"   = "green", "Unfenced" = "purple")) +
    labs(color = "Fencing")+  # Optional: rename legend title
    labs(
      x = "Treatment",
      #y = "Change in seedling density per ha",
      y = expression("Change in Seedling density "*ha^{-1}*"")
    ) +
    theme_classic()+ 
    ggtitle(NULL)+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)+
    theme(
      axis.title = element_text(size = 8),      # Axis titles
      axis.text = element_text(size = 8))
  
  
  
  ## Combine the plots in a single layout
  multi_panelsE <- (SeedViolin2/Seedbviolin2/ semm2) +   # "/" for stacking vertically, or "|" for side-by-side
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
  