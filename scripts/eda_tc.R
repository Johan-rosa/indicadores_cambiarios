library(dplyr)
library(tidyr)
library(lubridate)
library(highcharter)
library(logger)
library(reactable)

source(here::here("scripts/logic/get_tc.R"))

tc_mensual_fp <- get_tc_spot(frecuencia = "mensual", average_or_fp = "fp")
tc_mensual_prom <- get_tc_spot(frecuencia = "mensual", average_or_fp = "average")

tc_diario <- get_tc_spot(frecuencia = "diaria") |>
  mutate(
    year = year(fecha),
    mes  = month(fecha)
  ) |>
  relocate(year, mes, .after = fecha)


tc_eoy <- tc_diario |>
  filter(fecha == max(fecha), .by = year) |>
  select(year, venta_eoy = venta)

tc_to_plot <- tc_diario |>
  left_join(mutate(tc_eoy, year = year + 1)) |>
  mutate(
    venta_vd = (venta - venta_eoy) / venta_eoy
  ) |>
  mutate(
    day = row_number(),
    year = as.character(year),
    .by = year
  )


# -------------------------------------------------------------------------
log_info("Tipo de cambio de compra y venta del mercado spot")

plot_tc_spot <- tc_to_plot |>
  filter(year > 2024) |>
  select(fecha, compra, venta) |>
  pivot_longer(-fecha, names_to = "type", values_to = "tasa") |>
  mutate(type = stringr::str_to_title(type)) |>
  hchart(type = "line", hcaes(x = fecha, y = tasa, group = type)) |>
  hc_colors(c("#1d3557", "#e63946")) |>
  hc_plotOptions(
    series = list(
      marker = list(enabled = FALSE)
    ),
    line = list(
      states = list(
        inactive = list(opacity = 1)
      )
    )
  ) |>
  hc_title(text = "Tasa de cambio del mercado spot")


# -------------------------------------------------------------------------
log_info("Margen cambiario promedio")

plot_margen_promedio <- tc_mensual_prom |>
  mutate(
    marge = (venta - compra) / venta,
    date_label = format(fecha, "%b %Y")
  ) |>
  filter(year >= 2023) |>
  hchart(type = "column", hcaes(x = fecha, y = marge, date_label = date_label), name = "Margen") |>
  hc_tooltip(
    formatter = JS(
      "function() {
         return `<b>${this.point.date_label}:</b> ${(this.point.y * 100).toFixed(2)}%`;
       }"
    )
  ) |>
  hc_yAxis(
    title = list(text = NA),
    labels = list(
      formatter = JS("function(){ return (this.value * 100).toFixed(1) + '%'; }")
    )
  ) |>
  hc_colors("#457") |>
  hc_title(text = "Margen cambiario promedio") |>
  hc_xAxis(title = list(text = NA))


# -------------------------------------------------------------------------
log_info("Tasa de cambio acumulada en lo que va de año, según día laborable")

tasa_acumulada <- tc_to_plot |>
  # Para asegurar que se están graficando los últimos 4 años
  filter(year > (max(as.numeric(year)) - 4)) |>
  mutate(date_label = format(fecha, "%d %b")) |>
  hchart(
    type = "line", hcaes(x = day, y = venta_vd, group = year, date = date_label)
  ) |>
  hc_colors(c("#a8dadc", "#457b9d", "#1d3557", "#e63946")) |>
  hc_plotOptions(
    series = list(
      marker = list(enabled = FALSE)
    ),
    line = list(
      states = list(
        hover = list(enabled = FALSE),
        inactive = list(opacity = 1)
      )
    )
  ) |>
  hc_xAxis(
    title = list(text = "Día laborable del año")
  ) |>
  hc_yAxis(
    title = list(text = "Variación porcentual"),
    labels = list(
      formatter = JS("function(){ return (this.value * 100).toFixed(1) + '%'; }")
    )
  ) |>
  hc_tooltip(
    formatter = JS(
      "function() {
        return `
          <span style=\"color:${this.point.color}\">\u25CF</span>
          <b>${this.point.date}:</b> ${(this.point.y * 100).toFixed(1)}%
        `;
       }"
    )
  ) |>
  hc_title(text = "Variación acumulada del tipo de cambio de venta")

# Table cambiaria ---------------------------------------------------------

accuracy <- 0.01

table_data <- get_tc_from_banks() |>
  filter_out(stringr::str_detect(tipo, "Sucur")) |>
  filter(date == max(date)) |>
  filter(
    bank %in% c("BHD", "Banco Popular", "Banreservas", "Scotiabank")
  ) |>
  select(-c(tipo, date)) |>
  mutate(marge = sell - buy)


imgs <- tibble(
  bank = c(
    "BHD",
    "Banco Popular",
    "Banreservas",
    "Scotiabank"
  ),
  img = c(
    "https://www.infodolar.com.do/images/entidades/banco-bhd-2x.png",
    "https://popularenlinea.com/_catalogs/masterpage/popularenlinea/shared/images/BPD-logo.png",
    "https://www.infodolar.com.do/images/entidades/banreservas.svg",
    "https://do.scotiabank.com/content/dam/scotiabank/images/logos/2019/scotiabank-logo-red-desktop-200px.svg"
  ),
  h = c(rep(25, 3), 14)
)


tabla_tc <- imgs |>
  select(-h) |>
  left_join(table_data, by = "bank") |>
  relocate(sell, .before = buy) |>
  reactable(
    columns = list(
      img = colDef(
        name = "",
        cell = function(value, i) {
          htmltools::img(src = value, style = glue::glue("height: {imgs$h[i]}px;"), alt = value)
        }
      ),
      bank  = colDef(name = "Entidad", show = FALSE),
      sell  = colDef(name = "Tasa de venta", cell = \(x) scales::comma(x, accuracy)),
      buy   = colDef(name = "Tasa de compra", cell = \(x) scales::comma(x, accuracy)),
      marge = colDef(name = "Margen", cell = \(x) scales::comma(x, accuracy))
    )
  )







