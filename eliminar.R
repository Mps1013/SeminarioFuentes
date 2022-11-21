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

ggsave(
  filename = "Relación_Suicidios_Innacesibilidad_SaludMental.jpg",
  plot = suina_plot2,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Relación_Desigualdad_Innacesibilidad.jpg",
  plot = desina_plot2,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Relación_Suicidios_Innacesibilidad_AtencionMedica.jpg",
  plot = suina2_plot2,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Relación_Suicidios_Innacesibilidad_MedicamentoRecetado.jpg",
  plot = suina3_plot2,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Relación_Suicidios_Innacesibilidad_SaludMental_RL.jpg",
  plot = suina_plot2.1,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Relación_Suicidios_Innacesibilidad_AtencionMedica_RL.jpg",
  plot = suina2_plot2.1,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)

ggsave(
  filename = "Relación_Suicidios_Innacesibilidad_MedicamentoRecetado_RL.jpg",
  plot = suina3_plot2.1,
  path = "OUTPUT/DATA", # ruta relativa
  scale = 0.5,
  width = 40,
  height = 30,
  units = "cm",
  dpi = 320
)
