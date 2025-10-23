# =========================
# 0) Paquetes y semilla
# =========================

library(ABM)
set.seed(1)


# =========================
# 1) Parámetros del modelo
# =========================

N      <- 5000
Tmax   <- 36
delay_prob_base <- 0.10 # Mayor puntualidad en pagos (antes 0.30 -> 0.10, 90% puntuales)
lambda_cred     <- 0.70 # Mayor sensibilidad a los retrasos (antes 0.30 -> 0.70)

# 💔 ELIMINADO: transfer_amount (ya no usamos monto fijo)
# transfer_amount <- 600

# ----- Diseño del programa de transferencias ----- 
#       Monto por hijx y condiciones del programa 
#       Las tres líneas definen las reglas de elegibilidad 
#       y beneficio económico del programa tipo CCT.
base_transfer_per_child <- 800  # Aumento del monto por hijx (antes 500 -> 800)
max_children_paid       <- 2    # máximo número de hijxs cubiertos (2).
poverty_line            <- 4500 # umbral de ingreso del hogar para ser elegible (4500).
# Hogares con ingreso menor a ese monto son beneficiarios potenciales

# NUEVO: parámetros socioeconómicos adicionales
beta_age    <- 0.08      # efecto edad sobre salario esperado (tipo Mincer)
beta_school <- 0.12      # efecto grado escolar
cost_school <- 200       # Aumento del costo escolar (antes 100 -> 200)
urban_penalty <- 100     # Enfoque rural (mayor penalización de acceso; antes 50 -> 100)

# Estas lineas ⬆️ se usan luego en el cálculo de ✨utilidades  y salarios.
# - beta_age: cuánto aumenta el salario potencial del niñx por cada año de edad (8% aprox) - > lxs mayores tienen más empleabilidad.
# - beta_school: cuánto aumenta el salario esperado por cada grado escolar completado.
#                in(w) = b0 + b1*edad +b2*ecolaridad + error
#                w_child <- exp(log(100) + beta_age*edad + beta_school*grado + rnorm(1, 0, 0.2))

# - cost_school: costo percibido de asistir a la escuela ... reduce la utilidad neta de las opciones E y ET. 
# - urban_penalty: penalización del entorno rural sobre el umbral de decisión theta.
#                  En zonas rurales el costo o la dificultad para asistir es mayor, asi que se les suma al umbral
#                  haciendo menos probable que elijan asistir.


# =====================================
# 2) Crear simulación y estado inicial
# =====================================

sim <- Simulation$new(N) # Crea un objeto de clase Simulation del paquete ABM con N agentes.
#                          sim es la cajita donde viven los agentes y donde se van a guardar sus estados, atributos, contadores y eventos.
#                          Por ahora está vacía; solo sabe que tendrá 5000 espacios disponibles.
# 💔 ELIMINADO: seedA <- 10

for (i in 1:N) {                           # Recorre cada agente i (de 1 a 5000).                
  theta  <- rnorm(1, mean = 300, sd = 80)  # un umbral o inclinación individual para la decisión de asistir a la escuela.
  cred0  <- 1 - delay_prob_base            # CAMBIO 2.1: con delay_prob_base = 0.10, cred0 = 0.90 (antes 0.70 si 0.30)
  # Los agentes comienzan confiando 70% en que el programa cumple.
  # 💔 ELIMINADO: state0 <- if (i <= seedA) "A" else "N"
  state0 <- sample(c("E", "ET", "T"), 1, prob = c(0.4, 0.3, 0.3)) # El estado inicial se elige aleatoriamente para cada agente:
  # - 40% empiezan solo en escuela (E),
  # - 30% en escuela + trabajo (ET),
  # - 30% solo trabajo (T).
  # Esto define la situación de partida del modelo, antes de que actúen las reglas dinámicas (tick_handler).
  
  # Características del hogar: Hijxs, ingreso, elegibilidad (ya estaba)
  n_hijos  <- sample(0:3, 1, prob = c(0.25, 0.40, 0.25, 0.10)) # número de hijxs en el hogar. Sortea un número entero 0, 1, 2 o 3 con esas probabilidades (que suman 1).
  Y_hogar  <- rlnorm(1, meanlog = log(4000), sdlog = 0.5)      # genera un ingreso siempre positivo con una log-normal (muy usada para ingresos por su sesgo a la derecha).
  elegible <- (Y_hogar < poverty_line)                         # condición lógica. Si el ingreso es menor a la línea de pobreza (4500), el hogar califica para el programa.
  
  # NUEVO: características individuales/ambientales
  edad   <- sample(10:17, 1)                                    # edad siempre está entre 10 y 17.                       
  grado  <- max(1, edad - 6)                                    # si edad = 10 - > edad - 6 = 4 → max(1, 4) = 4                         
  sexo   <- sample(c("M", "F"), 1, prob = c(0.5, 0.5))          # elige un valor entre M y F con probabilidad 50%–50%
  zona   <- sample(c("urbana", "rural"), 1, prob = c(0.7, 0.3)) # elige un valor entre urbana y rural con probabilidades 70% y 30%
  if (zona == "rural") theta <- theta + urban_penalty           # CAMBIO 2.5: mayor penalización rural (urban_penalty=100)
  # theta funciona como un umbral / resistencia a la asistencia escolar.
  # Si es rural, hay más fricción (distancia, transporte, costos), así que incrementas ese umbral y hace menos atractiva la opción ir a la escuela a igualdad de condiciones.
  sim$setState(i, list(
    state0,
    theta     = theta,
    cred      = cred0,
    n_hijos   = n_hijos,
    Y_hogar   = Y_hogar,
    elegible  = elegible,
    edad      = edad,        # NUEVO
    grado     = grado,       # NUEVO
    sexo      = sexo,        # NUEVO
    zona      = zona         # NUEVO
  ))
}

# setState() fotografía el estado inicial del agente i y guarda su estado actual + atributos.
# El primer elemento es el estado (E, ET, T). Los demás son atributos nombrados que el motor lleva consigo en cada tick.


# ======================================
# 3) Loggers (contadores automáticos)
# ======================================

sim$addLogger(newCounter("E", "E"))   # Solo escuela  1) columna "E"   2) cuenta agentes en estado "E"
sim$addLogger(newCounter("ET", "ET")) # Escuela + trabajo
sim$addLogger(newCounter("T", "T"))   # Solo trabajo

# Los tres newCounter dan, en cada mes, cuántos hogares están en solo escuela (E), escuela+trabajo (ET) y solo trabajo (T), 
# y esa serie queda guardada en res para graficar o analizar.

# =========================================================
# 4) Handler mensual (la dinámica)
# =========================================================
tick_handler <- function(time, sim, agent) {  # Es una función que el motor de simulación ejecuta una vez por cada mes (tick).
  # recibe tres cosas: time: el mes actual (0, 1, 2, …, 36), sim: el objeto de simulación (donde viven los agentes), agent: el programador interno del motor (para reagendar el siguiente tick).
  for (i in 1:N) {                            # recorre los N hogares.
    ai <- getAgent(sim, i)                    # trae al agente i. Obtiene todos sus atributos guardados (estado, cred, edad, n_hijos, etc.).
    st <- getState(ai)                        # el nombre de una variable para guardar lo que devuelve getState(ai)
    
    # ai el agente que estoy manipulando
    # st los datos actuales de ese agente
    
    # 💔 ELIMINADO: benefit <- transfer_amount * st$cred
    
    # ----- Cálculo del beneficio -----
    #       Beneficio por hijx si elegible
    hijos_cubiertos <- min(st$n_hijos, max_children_paid) # Calcula cuántos hijxs reciben la transferencia (máximo 2).
    transfer_eff    <- if (st$elegible) base_transfer_per_child * hijos_cubiertos else 0  # Si el hogar es elegible, recibe monto * hijxs_cubiertos; si no, 0.
    
    # ----- Cálculo del salario potencial del niñx -----
    #       NUEVO: salario potencial del niñx (modelo tipo Mincer)
    w_child <- exp(log(100) + beta_age * st$edad + beta_school * st$grado + rnorm(1, 0, 0.2))  # Ecuación tipo Mincer: el salario crece con la edad y la escolaridad.
    
    # ----- Utilidades de las tres opciones -----
    # NUEVO: utilidades de tres opciones (E=solo escuela, ET=escuela y trabajo, T=solo trabajo)
    U_E  <- (transfer_eff * st$cred) - cost_school   -   st$theta
    # E (solo escuela): recibe toda la transferencia esperada (ajustada por credibilidad),
    # paga el costo completo de asistir (cost_school),
    # y enfrenta toda la barrera de asistencia (theta), que es mayor en zonas rurales.
    U_ET <- (transfer_eff/2) + (w_child * 0.5) - (cost_school/2)  - (0.5*st$theta)  
    # ET (escuela + trabajo): combina ambas actividades a medio tiempo.
    # Recibe solo la mitad de la transferencia (por asistencia parcial),
    # gana la mitad del salario (media jornada laboral),
    # paga la mitad del costo escolar,
    # y enfrenta solo la mitad de la barrera (theta), 
    # pues asiste parcialmente a la escuela y su logística es menos exigente.
    U_T  <- w_child
    # T (solo trabajo): no recibe transferencia ni paga costo escolar,
    # pero obtiene todo el salario potencial del trabajo.
    
    # ----- Decisión del hogar -----
    utilities <- c(E = U_E, ET = U_ET, T = U_T)  # junta las tres utilidades en un vector.
    new_state <- names(which.max(utilities))     # elige el estado que tenga la utilidad más alta (la mejor decisión económica para ese hogar). which.max() da la posición; names() extrae E, ET o T
    
    # ----- Ver si el pago llegó a tiempo -----
    # NUEVO: pagado a tiempo solo si recibe transferencia (E o ET)
    paid_on_time <- (new_state %in% c("E", "ET")) && (runif(1) > delay_prob_base) 
    # CAMBIO 2.1: con delay_prob_base = 0.10, la puntualidad esperada ~90%.
    # CAMBIO 2.2: por lambda_cred=0.70, cualquier retraso/ puntualidad impacta fuertemente la credibilidad.
    
    # ----- Actualizar credibilidad (EMA) -----
    cred_new <- (1 - lambda_cred) * st$cred + lambda_cred * as.numeric(paid_on_time)
    
    # Toma la credibilidad anterior del hogar: st$cred (un número entre 0 y 1).
    # Convierte el evento de hoy a número: as.numeric(paid_on_time)
    # Mezcla lo anterior con lo de hoy usando pesos:
    # - Peso de lo anterior: (1 - lambda_cred)
    # - Peso de lo de hoy: lambda_cred
    # Guarda el resultado en cred_new: nueva credibilidad = (peso a lo antiguo) × (credibilidad anterior) + (peso a lo de hoy) × (evento de hoy).
    
    # ----- Guardar nuevo estado y atributos -----
    #       Toma al agente que se esta actualizando (ai).
    #       Le guarda su nuevo estado y sus atributos en la simulación.
    setState(ai, list(
      new_state,
      theta     = st$theta,
      cred      = cred_new,
      n_hijos   = st$n_hijos,
      Y_hogar   = st$Y_hogar,
      elegible  = st$elegible,
      edad      = st$edad,
      grado     = st$grado,
      sexo      = st$sexo,
      zona      = st$zona
    ))
  }
  
  if (time < Tmax) schedule(agent, newEvent(time + 1, tick_handler))  # si todavía no llegamos al último mes (Tmax), programa que esta misma función (tick_handler) se ejecute otra vez en el siguiente mes
}


# ==============================
# 5) Ejecutar la simulación
# ==============================

schedule(sim$get, newEvent(0, tick_handler)) # esta línea arranca la simulación: es como presionar el botón play
res <- sim$run(0:Tmax)                       # sim$run() ejecuta todos los eventos que fueron programados
# 💔 ELIMINADO: res$attend <- res$A / N
res$attend <- (res$E + res$ET) / N           # Crea una nueva columna en res llamada attend (asistencia): Proporción que estudia (solo escuela o escuela+trabajo)

res

# ==============================
# 6) Gráfico
# ==============================
library(ggplot2)

p <- ggplot(res, aes(x = times, y = attend)) +
  geom_area(fill = "yellow2", alpha = 0.12) +
  geom_line(size = 1.4, color = "orange", lineend = "round") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "CCT: Decisión educativa (E / ET / T) con salarios y contexto",
       x = "Mes", y = "% hogares que asisten") +
  theme_minimal(base_size = 14)

p
