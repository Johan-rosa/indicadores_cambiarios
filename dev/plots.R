library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(highcharter)

box::use(scripts / logic / get_tc[get_tc_spot])

data_tc_mensual <- get_tc_spot(frecuencia = "mensual", average_or_fp = "fp")
tc_mensual_prom <- get_tc_spot(frecuencia = "mensual", average_or_fp = "average")

data_tc <- get_tc_spot(frecuencia = "diaria") |>
  mutate(
    year = year(fecha),
    mes  = month(fecha)
  ) |>
  relocate(year, mes, .after = fecha)


tc_eoy <- data_tc |>
  filter(fecha == max(fecha), .by = year) |>
  select(year, venta_eoy = venta)

tc_to_plot <- data_tc |>
  left_join(mutate(tc_eoy, year = year + 1)) |>
  mutate(
    venta_vd = (venta - venta_eoy) / venta_eoy
  ) |>
  mutate(
    day = row_number(),
    year = as.character(year),
    .by = year
  )

tc_before_covid <- tc_to_plot |>
  filter(year > 2010, year < 2020)

tc_to_plot |>
  filter(year > 2024) |>
  select(fecha, compra, venta) |>
  pivot_longer(-fecha, names_to = "type", values_to = "tasa") |>
  mutate(type = stringr::str_to_title(type)) |>
  hchart(type = "line", hcaes(x = fecha, y = tasa, group = type)) |>
  hc_colors(c("#1d3557", "#e63946"))


tc_to_plot

tc_mensual_prom |>
  mutate(
    marge = (venta - compra) / venta,
    date_label = format(fecha, "%b %Y")
  ) |>
  filter(year >= 2020) |>
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

tc_to_plot |>
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
  hc_title(text = "Variación acumulada del tipo de cambio de venta, según año")



tc_to_plot |>
  mutate(margen = venta - compra) |>
  select(fecha, year, mes, compra, venta, margen) |>
  filter(year > 2010) |>
  summarise(
    year_mes = max(round_date(fecha, unit = "month")),
    venta = mean(venta),
    margen_min = min(margen),
    margen_max = max(margen),
    margen = mean(margen),
    .by = c(year, mes)
  ) |>
  mutate(
    across(where(is.numeric), \(x) round(x, 3))
  ) |>
  hchart(
    type = "arearange",
    hcaes(
      x = year_mes,
      low = margen_min,
      high = margen_max,
    ),
    name = "Rango"
  )



mg_dt <- tc_to_plot |>
  mutate(margen = venta - compra) |>
  select(fecha, year, mes, compra, venta, margen) |>
  filter(year > 2010) |>
  summarise(
    year_mes = max(round_date(fecha, unit = "month")),
    venta = mean(venta),
    margen_min = min(margen),
    margen_max = max(margen),
    margen = mean(margen),
    .by = c(year, mes)
  ) |>
  mutate(
    across(where(is.numeric), \(x) round(x, 3))
  )



highchart() |>
  # Arearange
  hc_add_series(
    type = "arearange",
    data = mg_dt,
    hcaes(x = year_mes, low = margen_min, high = margen_max),
    name = "Rango",
    color = "#a8dadc",
    fillOpacity = 0.3
  ) |>
  # Línea promedio
  hc_add_series(
    type = "line",
    data = mg_dt,
    hcaes(x = year_mes, y = margen),
    name = "Margen promedio",
    color = "#1d3557",
    marker = list(enabled = FALSE)
  )

tc_to_plot |>
  mutate(margen = venta - compra) |>
  select(fecha, year, mes, compra, venta, margen) |>
  filter(year > 2010) |>
  summarise(
    year_mes = max(round_date(fecha, unit = "month")),
    venta = mean(venta),
    margen_min = min(margen),
    margen_max = max(margen),
    margen = mean(margen),
    .by = c(year, mes)
  ) |>
  mutate(across(contains("margen"), \(x) x / venta)) |>
  ggplot(aes(x = year_mes, y = margen)) +
  geom_ribbon(
    aes(ymin = margen_min, ymax = margen_max, fill = "Volatilidad mensual")
  ) +
  geom_line(color = "#e63946", size = 1) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = "gray") +
  labs(
    title = "Margen cambiario promedio como porcentaje de la tasa de venta",
    subtitle = "Frecuencia mensual, 2010 - 2025",
    fill = NULL,
    x = NULL,
    y =  NULL
  )
