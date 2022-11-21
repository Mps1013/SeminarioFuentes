ggsave(
  filename = "Relación_Suicidios_Desigualdad.jpg",
  plot = sude_plot2,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)