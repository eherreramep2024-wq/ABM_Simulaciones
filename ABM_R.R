# 0) Paquetes y semilla
# =====================

install.packages("ABM")
library(ABM)        # Carga el paquete ABM: define Simulation, Event, counters, etc.
set.seed(1)         # Fija la semilla para que los resultados sean reproducibles

# ========================
# 1) Parámetros del modelo
# ========================

N              <- 5000   # Número de hogares (cada uno es un "agente")
Tmax           <- 36     # Número de meses (ticks) a simular
transfer_amount <- 800   # Monto nominal mensual del programa ($800)
delay_prob_base <- 0.25  # Probabilidad de retraso del pago (25%)
lambda_cred     <- 0.35  # Qué tan rápido "aprende" la credibilidad (promedio móvil)

# Si crees que los hogares ajustan rápido su expectativa (mucha atención a la experiencia)

# ====================================
# 2) Crear simulación y estado inicial
# ====================================

# En ABM, el "estado" principal del agente será un string: "A" (asiste) o "N" (no asiste)
# Además guardamos atributos por agente:
# - theta: umbral individual de decisión
# - cred: credibilidad percibida de cobrar a tiempo (entre 0 y 1)

sim <- Simulation$new(N)  # Crea una simulación con N agentes (IDs 1..N ya existen internamente)

seedA <- 10  # Cantidad de hogares que ya comienzan asistiendo en t=0

for (i in 1:N) {  # Recorremos todos los agentes para fijar su estado inicial
  theta0 <- rnorm(1, mean = 400, sd = 100)   # Umbral heterogéneo ~ Normal(400, 1000)
  cred0  <- 1 - delay_prob_base             # Credibilidad inicial (ej.: 0.70)
  state0 <- if (i <= seedA) "A" else "N"    # Primeros 'seedA' agentes arrancan en "A"
  sim$setState(i, list(state0, theta = theta0, cred = cred0))
}

# setState(indice, lista):
#  - el primer elemento de la lista es el ESTADO ("A"/"N")
#  - el resto son atributos nombrados que viajan con el agente
# fija el estado del agente i como el primer elemento de la lista (A/N)
# y guarda atributos nombrados (theta, cred, etc.).

# ============================
# 3) Loggers (contadores automáticos)
# ============================

# newCounter(nombre, desde): crea un contador que, en cada tick, 
# cuenta cuántos agentes están en un cierto estado "desde".
# Aquí contamos cuántos están en "A" y cuántos en "N" en cada tiempo.

sim$addLogger(newCounter("A", "A"))  # cuántos asisten
sim$addLogger(newCounter("N", "N"))  # cuántos no asisten

#4

# ABM dispara funciones "handler" programadas como eventos.
# La firma SIEMPRE es: function(time, sim, agent)
#  - time: tiempo actual (mes)
#  - sim : el objeto simulación (para leer/escribir estados)
#  - agent: el agente al que se programó el evento (usaremos sim$get para re-agendar)

tick_handler <- function(time, sim, agent) {   # Recorremos TODOS los agentes y actualizamos su estado de este mes
  for (i in 1:N) {
    ai <- getAgent(sim, i)           # Obtiene el agente i
    st <- getState(ai)               # st[[1]] es "A" o "N"; luego atributos
    
    # Beneficio esperado este mes: monto * credibilidad percibida
    benefit   <- transfer_amount * st$cred
    
    # Regla de decisión mínima:
    #   Asiste si el beneficio esperado supera su umbral theta
    new_state <- if (benefit >= st$theta) "A" else "N"
    
    # Si asiste, “experimento” de pago a tiempo (Bernoulli con p = 1 - delay_prob_base)
    paid_on_time <- (new_state == "A") && (runif(1) > delay_prob_base)
    
    # Actualizar credibilidad (promedio móvil)
    cred_new <- (1 - lambda_cred) * st$cred + lambda_cred * as.numeric(paid_on_time)
    
    # Guardamos nuevo estado + atributos (con el theta que no cambia)
    setState(ai, list(new_state, theta = st$theta, cred = cred_new))
  }

  # Re-agendar el mismo handler para el mes siguiente, hasta Tmax
  # Usamos 'agent' (puntero del propio Simulation) como contenedor del evento recurrente
  if (time < Tmax) schedule(agent, newEvent(time + 1, tick_handler))
}  

# Programamos el primer “tick” en t = 0 sobre la entidad 'sim$get' (el agente "Simulación")
schedule(sim$get, newEvent(0, tick_handler))

# sim$run(0:Tmax) avanza la simulación y devuelve un data.frame con:
#   - columna temporal (times)
#   - columnas de cada logger (A, N)
res <- sim$run(0:Tmax)     # data.frame con columnas: time, A, N

# Creamos una columna con la proporción que asiste (A/N total)
res$attend <- res$A / N


library(ggplot2)

p <- ggplot(res, aes(x = times, y = attend)) +
  geom_area(fill = "yellow2", alpha = 0.12) +
  geom_line(size = 1.4, color = "orange", lineend = "round") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "CCT (umbral + credibilidad)", x = "Mes", y = "% que asiste") +
  theme_minimal(base_size = 14)

p



#2 Ejercicio con modificaciones e incremento de la transferencia final

# ========================
# 1) Parámetros del modelo
# ========================

N              <- 5000   # Número de hogares (cada uno es un "agente")
Tmax           <- 36     # Número de meses (ticks) a simular
transfer_start <- 600    # Transferencia inicial
transfer_end   <- 900    # Transferencia final (simulación gradual)
delay_prob_base <- 0.25  # Probabilidad de retraso del pago (25%)
lambda_cred     <- 0.35  # Qué tan rápido "aprende" la credibilidad (promedio móvil)

# Vector de transferencias mensuales
transfer_amount <- seq(transfer_start, transfer_end, length.out = Tmax + 1)

# ====================================
# 2) Crear simulación y estado inicial
# ====================================

# En ABM, el "estado" principal del agente será un string: "A" (asiste) o "N" (no asiste)
# Además guardamos atributos por agente:
# - theta: umbral individual de decisión
# - cred: credibilidad percibida de cobrar a tiempo (entre 0 y 1)

sim <- Simulation$new(N)  # Crea una simulación con N agentes (IDs 1..N ya existen internamente)

seedA <- 10  # Cantidad de hogares que ya comienzan asistiendo en t=0

for (i in 1:N) {  # Recorremos todos los agentes para fijar su estado inicial
  theta0 <- rnorm(1, mean = 400, sd = 100)   # Umbral heterogéneo ~ Normal(400, 100)
  cred0  <- 1 - delay_prob_base              # Credibilidad inicial (0.75)
  state0 <- if (i <= seedA) "A" else "N"     # Primeros 'seedA' agentes arrancan en "A"
  sim$setState(i, list(state0, theta = theta0, cred = cred0))
}

# ============================
# 3) Loggers (contadores automáticos)
# ============================

# newCounter(nombre, desde): crea un contador que, en cada tick, 
# cuenta cuántos agentes están en un cierto estado "desde".
# Aquí contamos cuántos están en "A" y cuántos en "N" en cada tiempo.

sim$addLogger(newCounter("A", "A"))  # cuántos asisten
sim$addLogger(newCounter("N", "N"))  # cuántos no asisten

# ============================
# 4) Evento principal (handler)
# ============================

tick_handler <- function(time, sim, agent) {   # Recorremos TODOS los agentes y actualizamos su estado de este mes
  for (i in 1:N) {
    ai <- getAgent(sim, i)           # Obtiene el agente i
    st <- getState(ai)               # st[[1]] es "A" o "N"; luego atributos
    
    # Transferencia correspondiente al mes actual (simulación gradual)
    T_t <- transfer_amount[time + 1]
    
    # Beneficio esperado este mes: monto * credibilidad percibida
    benefit <- T_t * st$cred
    
    # Regla de decisión mínima:
    #   Asiste si el beneficio esperado supera su umbral theta
    new_state <- if (benefit >= st$theta) "A" else "N"
    
    # Si asiste, “experimento” de pago a tiempo (Bernoulli con p = 1 - delay_prob_base)
    paid_on_time <- (new_state == "A") && (runif(1) > delay_prob_base)
    
    # Actualizar credibilidad (promedio móvil)
    # Si no asiste, mantenemos su credibilidad (no la dejamos caer a 0)
    cred_new <- if (new_state == "A") {
      (1 - lambda_cred) * st$cred + lambda_cred * as.numeric(paid_on_time)
    } else {
      st$cred
    }
    
    # Guardamos nuevo estado + atributos (theta se mantiene constante)
    setState(ai, list(new_state, theta = st$theta, cred = cred_new))
  }
  
  # Re-agendar el mismo handler para el mes siguiente, hasta Tmax
  if (time < Tmax) schedule(agent, newEvent(time + 1, tick_handler))
}

# Programamos el primer “tick” en t = 0 sobre la entidad 'sim$get' (el agente "Simulación")
schedule(sim$get, newEvent(0, tick_handler))

# ============================
# 5) Ejecutar simulación
# ============================

res <- sim$run(0:Tmax)     # data.frame con columnas: time, A, N

# Prop. que asiste (A/N total)
res$attend <- res$A / N
res$transfer <- transfer_amount[res$times + 1]

# ============================
# 6) Gráfico de resultado
# ============================

library(ggplot2)

ggplot(res, aes(x = times, y = attend)) +
  geom_area(fill = "yellow2", alpha = 0.12) +
  geom_line(size = 1.4, color = "orange", lineend = "round") +
  geom_line(aes(y = (transfer - min(transfer)) / diff(range(transfer))), 
            color = "blue", linetype = "dashed", size = 1) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "CCT (umbral + credibilidad) — Simulación gradual",
    subtitle = "Curva naranja: % que asiste | Línea azul: Transferencia progresiva",
    x = "Mes",
    y = "% que asiste"
  ) +
  theme_minimal(base_size = 14)
