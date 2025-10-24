# =========================
# 0) Paquetes
# =========================
library(dplyr)
library(ggplot2)

# =========================
# 1) Parámetros y diseño del modelo migratorio
# =========================

poverty_line     <- 5500   # umbral de ingreso mensual para presión migratoria
lambda_cred      <- 0.50   # memoria/actualización de expectativas migratorias
delay_prob_base  <- 0.35   # fricción/obstáculos base (trámites, redadas, clima)
cost_school      <- 600    # costo migratorio mensualizado (deuda, manutención, etc.)

season_vec <- c(              # Prob. detención estacional (ene..dic)
  -0.02, -0.01,  0.03,  0.04,   # ene, feb, mar, abr
  0.05,  0.04,  0.02,  0.02,   # may, jun, jul, ago
  0.03,  0.03,  0.00, -0.04    # sep, oct, nov, dic
)

# =========================
# 2) Construcción de variables derivadas (heterogeneidad inicial)
#  - Se asume que ya existe un data.frame 'hogares' con columnas:
#    ingreso_hogar, zona, edad, grado, n_hijos, sexo, etc.
# =========================

hogares <- hogares %>%
  mutate(
    elegible    = ingreso_hogar < poverty_line,          # Potenciales migrantes (presión económica)
    cred0       = 1 - delay_prob_base,                   # Creencia inicial de "éxito sin detención"
    theta       = ifelse(zona == "rural",
                         rnorm(n(), mean = 220, sd = 70),  # umbral/aversión al riesgo (rural más bajo)
                         rnorm(n(), mean = 300, sd = 90)),
    salario_pot = exp(log(200) + 0.06 * edad + 0.10 * grado + rnorm(n(), 0, 0.2)), # Mincer-like
    state0      = sample(c("E", "ET", "T"), n(), prob = c(0.70, 0.10, 0.20), replace = TRUE)
  )

# =========================
# 3) Crear simulación usando tu base
#  - Se asume disponible una clase Simulation con:
#    Simulation$new(N), setState(i, list(...)), get(i), run(0:Tmax),
#    y funciones globales schedule(agent, newEvent(...)), newCounter, newEvent
# =========================
N    <- nrow(hogares)      # Número de agentes
Tmax <- 36                 # Horizonte (meses)
sim  <- Simulation$new(N)

for (i in 1:N) {
  sim$setState(i, list(
    hogares$state0[i],           # <- Estado (posición 1, SIN nombre): "E","ET","T"
    theta    = hogares$theta[i],
    cred     = hogares$cred0[i],
    n_hijos  = hogares$n_hijos[i],
    Y_hogar  = hogares$ingreso_hogar[i],
    elegible = hogares$elegible[i],
    edad     = hogares$edad[i],
    grado    = hogares$grado[i],
    salario  = hogares$salario_pot[i],
    zona     = hogares$zona[i],
    sexo     = hogares$sexo[i]
  ))
}

# =========================
# 4) Dinámica mensual (handler CORREGIDO)
# =========================
tick_handler <- function(time, sim, agent) {
  # ---- Capturar estado actual del agente ----
  ai <- agent
  st <- getState(ai)
  
  # ----- Contexto mensual: prob. de éxito sin detención -----
  t <- as.integer(time)
  # season_vec es “prob. detención estacional”; se usa con signo inverso para “éxito sin detención”
  season_eff <- -season_vec[(t %% 12) + 1]
  policy_eff <- if (t >= 6 && t <= 18) -0.08 else if (t >= 24 && t <= 28) -0.04 else 0
  shock_eff  <- rnorm(1, mean = 0, sd = 0.02)
  base_pub   <- 1 - delay_prob_base
  p_public_tick <- max(0, min(1, base_pub + season_eff + policy_eff + shock_eff))
  
  # ----- Valores seguros -----
  zona_val     <- tryCatch(as.character(st$zona)[1], error = function(e) NA_character_)
  is_rural     <- isTRUE(zona_val == "rural")
  n_hijos_val  <- ifelse(is.null(st$n_hijos) || is.na(st$n_hijos), 0, st$n_hijos)
  elegible_val <- isTRUE(st$elegible)
  salario_val  <- ifelse(is.null(st$salario) || is.na(st$salario), 0, st$salario)
  theta_val    <- ifelse(is.null(st$theta)   || is.na(st$theta),   0, st$theta)
  cred_val     <- ifelse(is.null(st$cred)    || is.na(st$cred),    0.5, st$cred)
  
  # Éxito percibido por el hogar (penaliza rural 25 p.p.)
  p_public <- if (is_rural) max(0, min(1, p_public_tick - 0.25)) else p_public_tick
  
  # ----- Beneficios / ingresos esperados -----
  hijos_cubiertos <- min(n_hijos_val, 2)
  transfer_eff    <- if (elegible_val) 300 * hijos_cubiertos else 0
  w_child         <- salario_val
  
  # ----- Utilidades (E=quedarse; ET=intento; T=migrar) -----
  U_E  <- (transfer_eff * p_public)                     - cost_school - theta_val
  U_ET <- (transfer_eff/2) + (p_public * w_child * 0.5) - (cost_school/2) - (0.5 * theta_val)
  U_T  <- (p_public * w_child)                          - cost_school - theta_val
  utilities <- c(E = U_E, ET = U_ET, T = U_T)
  utilities[!is.finite(utilities)] <- -1e9
  
  # ----- Softmax robusto -----
  tau <- 400
  u_center <- utilities - max(utilities)
  weights <- exp(u_center / tau)
  weights[!is.finite(weights) | is.na(weights)] <- 0
  s <- sum(weights)
  
  if (s <= 0) {
    new_state <- names(which.max(utilities))
  } else {
    probs <- weights / s
    new_state <- sample(c("E","ET","T"), size = 1, prob = probs)
  }
  
  # ----- Actualizar credibilidad -----
  if (new_state %in% c("ET","T")) {
    paid_on_time <- runif(1) < p_public
    cred_new <- (1 - lambda_cred) * cred_val + lambda_cred * as.numeric(paid_on_time)
  } else {
    cred_new <- (1 - lambda_cred) * cred_val + lambda_cred * p_public_tick
  }
  
  # ----- Guardar -----
  setState(ai, list(
    as.character(new_state),   # <- Estado (posición 1, SIN nombre)
    theta    = theta_val,
    cred     = cred_new,
    n_hijos  = n_hijos_val,
    Y_hogar  = st$Y_hogar,
    elegible = elegible_val,
    edad     = st$edad,
    grado    = st$grado,
    salario  = salario_val,
    zona     = zona_val,
    sexo     = st$sexo
  ))
  
  # Re-agendar siguiente tick del mismo agente
  if (time < Tmax) {
    schedule(agent, newEvent(time + 1, tick_handler))
  }
}  # <-- cierre correcto de la función

# =========================
# 5) Ejecutar
# =========================

# Loggers de conteo por estado
# Loggers de conteo por estado
sim$addLogger(newCounter("E","E"))
sim$addLogger(newCounter("ET","ET"))
sim$addLogger(newCounter("T","T"))

# --- Helper: obtener "handle" del agente i según distintas APIs comunes ---
get_agent <- function(sim, i) {
  # 1) Métodos habituales
  try_methods <- c("getAgent", "agent", "get_agent", "GetAgent")
  for (m in try_methods) {
    if (m %in% names(sim) && is.function(sim[[m]])) {
      ai <- try(sim[[m]](i), silent = TRUE)
      if (!inherits(ai, "try-error")) return(ai)
    }
  }
  # 2) Campos habituales donde viven los agentes
  try_fields <- c("agents", ".agents", "_agents", "AGENTS")
  for (fld in try_fields) {
    if (fld %in% names(sim)) {
      aa <- sim[[fld]]
      if (is.list(aa) && length(aa) >= i) return(aa[[i]])
      if (is.environment(aa) && exists(as.character(i), aa, inherits = FALSE)) {
        return(get(as.character(i), aa, inherits = FALSE))
      }
    }
  }
  # 3) Algunas implementaciones aceptan el ID numérico directamente en schedule()
  #    Devolvemos i como "agente" y dejamos que el scheduler lo acepte si es válido.
  return(i)
}

# --- Helper: agendar con varios "sabores" de schedule() ---
schedule_agent0 <- function(target, time, fun) {
  # Variante 1: schedule(agente, newEvent(...))
  ok <- try(schedule(target, newEvent(time, fun)), silent = TRUE)
  if (!inherits(ok, "try-error")) return(invisible(TRUE))
  # Variante 2: schedule(sim, newEvent(..., target = i))
  ok2 <- try(schedule(sim, newEvent(time, fun, target = target)), silent = TRUE)
  if (!inherits(ok2, "try-error")) return(invisible(TRUE))
  # Variante 3: schedule(sim, i, newEvent(...))
  ok3 <- try(schedule(sim, target, newEvent(time, fun)), silent = TRUE)
  if (!inherits(ok3, "try-error")) return(invisible(TRUE))
  stop("No pude agendar con ninguna variante de schedule().")
}

# --- Agendar primer tick para cada agente ---
failed <- integer(0)
for (i in 1:N) {
  ai <- get_agent(sim, i)
  ok <- try(schedule_agent0(ai, 0, tick_handler), silent = TRUE)
  if (inherits(ok, "try-error")) failed <- c(failed, i)
}

# Si hubo fallas, mostramos diagnóstico útil y detenemos con mensaje claro
if (length(failed) > 0) {
  cat("\n[Diagnóstico schedule] No pude agendar a", length(failed), "agentes.\n")
  cat("Nombres en 'sim':\n"); print(names(sim))
  cat("\nClases de 'sim':\n"); print(class(sim))
  cat("\nEstructura de primer nivel de 'sim':\n"); str(sim, max.level = 1)
  stop("Revisa el diagnóstico anterior: ajusta el helper para la API específica de tu Simulation.")
}

# --- Ejecutar y construir métrica ---
res <- sim$run(0:Tmax)
res$attend <- (res$ET + res$T) / N
# =========================
# 6) Graficar
# =========================
ggplot(res, aes(x = times, y = attend)) +
  geom_line(size = 1.3, color = "steelblue") +
  geom_area(alpha = 0.15, fill = "skyblue") +
  geom_vline(xintercept = c(6,18,24,28), linetype = "dashed", color = "red") +  # ← aquí se agrega
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Migración simulada con base real de hogares",
    subtitle = "periodos de control migratorio (ICE/Trump)",
    x = "Mes",
    y = "% de hogares que intentan o logran migrar"
  ) +
  theme_minimal(base_size = 14)

  