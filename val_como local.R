# Cargar librerías
library(rvest)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggtext)
library(glue)
library(paletteer)
library(prismatic)


# logos y colores ----------------------------------------------------------------------------
tm_acb <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/logos_cuadrados_acb/refs/heads/main/acb_df.csv") %>%
  select(nameTeam = equipo, abb, color, logo)



# caption ------------------------------------------------------------------------------------
checkmas <- "<span style='color:#46e01d;font-family: \"Font Awesome 6 Brands\"'>&#x2713;</span>"
twitter <- "<span style='color:#c04719;font-family: \"Font Awesome 6 Brands\"'>&#xE61A;</span>"
tweetelcheff <- "<span style='font-weight:bold;'>*@elcheff*</span>"
insta <- "<span style='color:#E1306C;font-family: \"Font Awesome 6 Brands\"'>&#xE055;</span>"
instaelcheff <- "<span style='font-weight:bold;'>*@sport_iv0*</span>"
github <- "<span style='color:#4078c0;font-family: \"Font Awesome 6 Brands\"'>&#xF092;</span>"
githubelcheff <- "<span style='font-weight:bold;'>*IvoVillanueva*</span>"
caption <- glue("**Gráfico**: *Ivo Villanueva* • {twitter} {tweetelcheff}  para {twitter} *@SuperManagerACB*")

# calendario --------------------------------------------------------------
url <- "https://www.acb.com/calendario/index/temporada_id/2024/edicion_id/975/jornada_id/5837" %>%
  read_html()

jor <- tibble(
  local_abb = url %>% html_elements(".local span.abreviatura") %>%
    html_text(),
  local = url %>% html_elements(".local span.nombre_largo") %>%
    html_text(),
  visitante_abb = url %>% html_elements(".visitante span.abreviatura") %>%
    html_text(),
  visitante = url %>% html_elements(".visitante span.nombre_largo") %>%
    html_text()
)

# valoracion recibida por posición ----------------------------------------

pos <- c("Bases", "Aleros", "Pívots")
recPosloc <- function(pos) {
  url <- "https://www.rincondelmanager.com/smgr/valoracion_rec.php"
  pgsession <- session(url)
  pgform <- html_form(pgsession)[[5]]
  filled_form <- html_form_set(pgform,
                               "de_jor" = "1",
                               "a_jor" = "18",
                               "cancha" = "1"
  )

  df <- submit_form(session = pgsession, form = filled_form, POST = url)
  df <- df %>%
    html_element(glue::glue("table#t{pos}")) %>%
    html_table()|>
    janitor::clean_names() |>
    rename(eq =equipo) %>%
    mutate(
      eq = case_when(
        eq == "CBG" ~ "COV",
        eq == "OBR" ~ "MOB",
        eq == "RMA" ~ "RMB",
        eq == "BAS" ~ "BKN",
        eq == "FCB" ~ "BAR",
        eq == "CAN" ~ "LLT",
        eq == "ZAR" ~ "CAZ",
        eq == "BLB" ~ "SBB",
        eq == "MUR" ~ "UCM",
        eq == "MAN" ~ "BAX",
        eq == "PAL" ~ "ZPA",
        eq == "GCA" ~ "DGC",
        eq == "AND" ~ "MBA",
        TRUE ~ eq),
      position = paste0(pos),
      tipo = "visitante") %>%
    select(eq, media, position, tipo) %>%
    separate(media, c("media_total", "media_player1"), sep = "( )") %>%
    mutate_at(vars("media_total", "media_player1"), list(~ str_replace(., ",", "."))) %>%
    mutate(
      media_player1 = str_remove_all(media_player1, "[( )]"),
      across(c("media_total", "media_player1"), as.numeric)
    ) %>%
    select(-media_total)



  return(df)
}


recPosvis_df <- map_df(pos, recPosloc)

# dataset de posiciones de jugadores en SuperManager
posicion <- readRDS(here::here("2025", "superManager", "rds", "superManager.rds")) %>%
  glimpse() %>%
  select(shortName, fullName, nameTeam, up15, keep, down15, price, number, license, fisicStatus, competitionAverage, position, photo)

# Cargar datos de partidos locales y visitantes desde PlayByPlay
local_away <- readRDS(here::here("2025", "PlayByPlay", "acb2425", "rds", "iddf_df25.rds")) %>%
  select(id_match = id, id_phase, team_home = local_team_team_actual_name,
         team_away = visitor_team_team_actual_name,  matchweek_number) %>%
  pivot_longer(cols = starts_with("team"), names_to = "tipo", names_prefix = "team_", values_to = "nameTeam") %>%
  mutate(matchweek_number = as.numeric(matchweek_number)) %>%
  select(numberJourney = matchweek_number, nameTeam,cancha = tipo)

# Cargar datos de jugadores de SuperManager y cruzarlos con partidos
players_superM <- readRDS(here::here("2025", "superManager", "rds", "players_superM_Df.rds")) %>%
  glimpse() %>%
  select(shortName, bonusVictory, nameTeam, numberJourney) %>%
  left_join(local_away, join_by(numberJourney, nameTeam))

# Filtrar y procesar datos para obtener estadísticas de jugadores en partidos away
df_local <- players_superM %>%
  filter(nameTeam %in% jor$local & cancha == "home") %>%
  group_by(shortName, nameTeam) %>%
  slice_max(order_by = numberJourney, n = 5) %>%
  group_by(shortName, nameTeam) %>% # Agrupar por juga
  summarise(
    jug = sum(!is.na(bonusVictory)),
    tot_val = sum(bonusVictory, na.rm = TRUE),
    media_val = mean(bonusVictory, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(., posicion, join_by(shortName, nameTeam)) %>%
  filter(jug > 2 & fisicStatus == "fit") %>%
  arrange(desc(media_val)) %>%
  group_by(position) %>%
  slice(1:5) |>
  ungroup() |>
  mutate(position = case_when(
    position == 1 ~ "Bases",
    position == 3 ~ "Aleros",
    TRUE ~ "Pívots"
  )) %>%
  arrange(match(position, c("Bases", "Aleros", "Pívots"))) %>%
  left_join(., jor %>% select(nameTeam = local, against = visitante), join_by(nameTeam)) %>%
  left_join(., tm_acb, join_by(nameTeam)) %>%
  left_join(., tm_acb %>% select(against = nameTeam, abb_against = abb, logo_against = logo), join_by(against)) %>%
  left_join(recPosvis_df %>% filter(eq %in% jor$visitante_abb) %>% rename(abb_against = eq), join_by(abb_against, position)) %>%
  mutate(
    fullName = ifelse(photo == "0000572556_5-6_03.jpg", "Jaime Fernández M.", fullName),
    idfoto = str_extract(photo, "[0-9_+-]+"),
    fullName = str_squish(fullName),
    idfoto = ifelse(fullName == "Joaquín Rodríguez", "0000565882_5-6_03", idfoto),
    fotoPNG = paste0("https://raw.githubusercontent.com/IvoVillanueva/fotosSM2425/refs/heads/main/Fondo%20de%20%E2%80%9C", idfoto, "%E2%80%9D%20eliminado.png"),
    border_color = after_scale(clr_darken(color, 0.7)),
    fill_color = after_scale(clr_darken(color, 0.2)),
    name1 = word(fullName, 1), # Extraer la primera palabra
    name2 = word(fullName, 2, -1),
    number = paste0("#", number),
    fotoPNG = ifelse(fullName == "Dae Dae Grant", "C:/Users/iVo/Downloads/78786-1-5f5e8bf1bd3f12.PNG", fotoPNG),
    bandera = case_when(
      license == "EXT" ~ "https://www.rincondelmanager.com/imgs/bextra.gif",
      license == "JFL" ~ "https://www.rincondelmanager.com/imgs/bnac.gif",
      TRUE ~ ""
    )
  ) %>%
  glimpse()

# funciones ----------------------------------------------------------------------------------
add_photo_frame <- function(color, border_color, fotoPNG) {
  glue::glue(
    " <img style='
         width: 86px;
         height: 86px;
         border-radius: 20%;
         background-color:{color};
         margin-top: 8px!important;
         align-items: center;
         border: 2px solid{border_color};' src='{fotoPNG}'/>
    </div>"
  )
}


combine_word <- function(name1, name2, logo, eq, color, number) {
  glue::glue(
    "<div style='text-align: left; line-height: 28px; display: flex; flex-direction: column;'>
      <span style='font-weight: 200; font-variant: small-caps; font-size: 25px;'>{name1}</span>
      <span style='font-weight: 700; font-variant: small-caps; font-size: 25px;'>{name2}</span>
      <div style='
        text-align: left;
        margin-top: 4px!important;
        display: flex;
        align-items: center;'>
        <img style='
          height: 22px;
          width: auto;
          max-width: none;
          margin-right: 2px;'
          src='{logo}'/>
        <span style='font-weight: bold; color: grey; font-size: 14px;'>{eq} • <span style='color: {color};'>{number}</span></span>
      </div>
    </div>"
  )
}


# gt table -----------------------------------------------------------------------------------

path <- here::here("2025", "superManager", "basicos", "laboratorio valoracion como local y visitante", "png", "topvalcomoloc_19")

base <- recPosloc_df %>%
  filter(position == "Bases") %>%
  pull(media_player1)

alero <- recPosloc_df %>%
  filter(position == "Aleros") %>%
  pull(media_player1)

pivot <- recPosloc_df %>%
  filter(position == "Pívots") %>%
  pull(media_player1)

df_local %>%
  mutate(
    combo = combine_word(name1, name2, logo, nameTeam, color, number),
    combo = map(combo, gt::html),
    combo_img = add_photo_frame(color, border_color, fotoPNG),
    combo_img = map(combo_img, gt::html)
  ) %>%
  select(combo_img, combo, position, bandera, price, tot_val, jug, media_val, competitionAverage, up15, keep, down15, logo_against, media_player1) %>%
  gt(groupname_col = "position") %>%
  gt_img_rows(height = 25, bandera) %>%
  gt_img_rows(height = 95, logo_against) %>%
  cols_label(
    combo_img = "",
    combo = "Jugador",
    bandera = "",
    price = "BROKER",
    tot_val = "TOT.LOC",
    jug = "JUG",
    media_val = "MED.LOC",
    competitionAverage = "VAL.M",
    up15 = "+15%",
    keep = "0",
    down15 = "-15%",
    logo_against = "RIVAL",
    media_player1 = "VAL.REC"
  ) %>%
  cols_align(
    align = "left",
    columns = c(combo, bandera)
  ) %>%
  cols_align(
    align = "right",
    columns = c(combo_img)
  ) %>%
  cols_align(
    align = "center",
    columns = c(price:media_player1)
  ) %>%
  # Aplicar colores por grupo
  data_color(
    columns = media_val,
    palette = c("white", "#3494dc")
  ) %>%
  data_color(
    columns = media_player1,
    rows =1:5,
    fn = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = range(base, na.rm = TRUE), # Calcula el rango correctamente
      na.color = "#005C55FF" # Color para valores faltantes
    )
  ) %>%
  data_color(
    columns = media_player1,
    rows =6:10,
    fn = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = range(alero, na.rm = TRUE), # Calcula el rango correctamente
      na.color = "#005C55FF" # Color para valores faltantes
    )
  ) %>%
  data_color(
    columns = media_player1,
    rows =11:15,
    fn = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "Redmonder::dPBIRdGn",
        direction = 1
      ) %>% as.character(),
      domain = range(pivot, na.rm = TRUE), # Calcula el rango correctamente
      na.color = "#005C55FF" # Color para valores faltantes
    )
  ) %>%
  fmt_currency(
    placement = "right",
    columns = c(price),
    currency = "EUR",
    use_subunits = FALSE,
    sep_mark = "."
  ) %>%
  gtExtras::gt_theme_espn() %>%
  tab_options(
    heading.align = "left",
    table.font.names = "Oswald",
    table.background.color = "white",
    table.font.size = 42,
    data_row.padding = px(1),
    source_notes.font.size = 20,
    table.additional_css = ".gt_table {
                margin-bottom: 90px;
                margin-top: 90px;
              }"
  ) %>%
  # cols_width(
  #   # Especificar el ancho de las columnas
  #   columns = c(combo_img,tot_val:media_player1) ~ px(100),        # Establecer el ancho de la columna A
  #               columns = c(combo,price) ~ px(150),       # Establecer el ancho de la columna B
  #                           columns = c(bandera) ~ px(30)        # Establecer el ancho de la columna C
  # ) %>%

  tab_header(
    title = md("<div style='line-height:134px;vertical-align:middle;text-align:left;font-weight:600;font-size:64px'>
                 <img src='https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Logo%20SM%20mosca%20340x340.png'style='width:134px; height:134px;vertical-align:middle;padding-right:12px'
               <span style='text-align:left;'>Top 15 Como Local</div>"),
    subtitle = md("<span style='font-weight:400;color:#8C8C8C;font-size:32px'>Jugadores que han disputado al menos 3 de los últimos 5 partidos de su equipo como local | SuperManager 24/25</span>")
  ) %>%
  tab_source_note(
    source_note = md(paste0("**Datos**: *@SuperManagerACB*&nbsp;&nbsp; <img src='https://raw.githubusercontent.com/IvoVillanueva/acb_logo/main/Logo%20SM%20mosca%20340x340.png'
                     style='height:25px;width:25px;vertical-align:middle;'>&nbsp;&nbsp;<br>", caption))
  ) %>%
  gtsave(glue::glue("{path}.png"), vwidth = 3000, vheight = 200, expand = 300)
