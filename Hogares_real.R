# =========================
# 0) Paquetes y datos
# =========================

library(ABM)
library(dplyr)     # para crear columna derivadas
library(ggplot2)
library(readxl)

# Leer base real

head(hogares)

#Linea de pobreza sube a 4,500
#lambda_cred sube a 0.30
#Prob. de retraso sube a 0.20
#Costo de asistir pasa a 120

# =========================
# 1) Parámetros y diseño del programa de transferencias
# =========================

poverty_line <- 4000    # umbral de ingreso del hogar para ser elegible (4500).
# Hogares con ingreso menor a ese monto son beneficiarios potenciales
lambda_cred  <- 0.30    # velocidad de aprendizaje o memoria en esa credibilidad.
# Controla qué tanto se ajusta la confianza según la experiencia reciente:
# - Si lambda_cred = 1, el hogar olvida todo el pasado y solo cuenta el último pago.
# - Si lambda_cred = 0, nunca cambia su credibilidad (memoria infinita).
# - 0.30 significa que se ajusta 30% a lo nuevo y conserva 70% del valor anterior.
delay_prob_base <- 0.20 # probabilidad base de que el pago del programa llegue tarde (30%).
cost_school <- 120       # costo promedio mensual de asistir a la escuela (transporte, materiales, etc.)


season_vec <- c(              # Efectos estacionales  (ene..dic)
  -0.04, -0.03,  0.01,  0.02,   # ene, feb, mar, abr  → retrasos al inicio, mejora ligera
  0.03,  0.04, -0.05, -0.05,   # may, jun, jul, ago  → buen funcionamiento, pero baja en vacaciones
  -0.02, -0.01,  0.03,  0.04    # sep, oct, nov, dic  → lluvias, luego repunte y cierre puntual
)
# Es un vector de 12 números, uno por mes (enero a diciembre).
# Cada número es un ajuste en puntos de probabilidad sobre la puntualidad del programa ese mes.
# -0.03 = restar 3 puntos porcentuales a la puntualidad.
# +0.02 = sumar 2 puntos porcentuales.
# 0.00 = sin cambio estacional.


# =========================
# 2) Construcción de variables derivadas (heterogeneidad inicial)
# =========================

hogares <- hogares %>%
  mutate(
    elegible = ingreso_hogar < poverty_line,               # Nueva columna lógica (TRUE/FALSE): se elige por ingreso comparado con poverty_line.
    cred0    = 1 - delay_prob_base,                        # el hogar arranca creyendo que 70% de las veces el pago llega puntual.
    theta    = ifelse(zona == "rural",
                      rnorm(n(), mean = 350, sd = 80),     # Umbral de decisión heterogéneo (theta) por zona: Si zona: rural (media 250), sino urbana/otra (media 200)
                      rnorm(n(), mean = 200, sd = 80)),
    salario_pot = exp(log(100) + 0.08 * edad + 0.12 * grado + rnorm(n(), 0, 0.2)),       # Salario potencial estimado con un modelo tipo Mincer
    state0 = sample(c("E", "ET", "T"), n(), prob = c(0.4, 0.3, 0.3), replace = TRUE)     # Estado inicial (E 40%, ET 30%, T 30%)
  )

head(hogares)

# Objeto hogares ahora tiene columnas adicionales:
# - elegible (TRUE/FALSE),
# - cred0 (num, 0.7),
# - theta (num, dep. de zona),
# - salario_pot (num, log-normal según edad/grado),
# - state0 (factor/char con E/ET/T)
# Estas columnas no simulan nada aún; solo preparan la heterogeneidad inicial que luego usará tu simulación ABM (en el tick_handler) para decidir mes a mes.

# Las cargas a cada agente con setState(...).
# El tick_handler las usa para calcular utilidades y actualizar el estado/credibilidad mes a mes.


# =========================
# 3) Crear simulación usando tu base
# =========================
N <- nrow(hogares)            # Define cuántos agentes habrá en el mundo simulado (uno por fila de hogares, en esta versión).
Tmax <- 36                    # Número de pasos de tiempo (meses) que correrá la simulación.
sim <- Simulation$new(N)      # Crea el objeto simulación con capacidad para N agentes.

for (i in 1:N) {              # Inicia un bucle que va del agente 1 al N. i es el índice del agente (ID interno en sim)
  sim$setState(               # Asigna el estado y atributos del agente i.
    i,
    list(
      hogares$state0[i],
      theta   = hogares$theta[i], # Si el dato ya existe en una columna con N filas, necesitas [i].
      cred    = hogares$cred0[i],
      n_hijos = hogares$n_hijos[i],
      Y_hogar = hogares$ingreso_hogar[i],
      elegible = hogares$elegible[i],
      edad     = hogares$edad[i],
      grado    = hogares$grado[i],
      salario  = hogares$salario_pot[i],
      zona     = hogares$zona[i],
      sexo     = hogares$sexo[i]
    )
  )
}

# Importante: aquí no hay dinámica todavía. Solo estamos instalando el agente con su estado y mochila de atributos. 
# La dinámica vendrá luego en tu tick_handler (donde mes a mes cambias estado, actualizas cred, etc.).


# =========================
# 4) Dinámica mensual
# =========================

# p_public: Cada mes es qué tan puntual fue el programa ese mes en promedio, un número entre 0 y 1, donde 1 = 100% puntual.
# Ese número sale de sumar cuatro ideas:
# - Nivel base: lo normal del programa si nada cambia (por ejemplo, 0.70 si suele cumplir 70%).
# - Estacionalidad (season_vec): meses donde es peor (resta puntos) o mejor (suma puntos) por razones del calendario.
# - Política puntual (policy_eff): una mejora temporal (por ejemplo, durante 4 meses sube 10 puntos porcentuales).
# - Ruido del mes (shock_eff): lo impredecible (un poquito para arriba o abajo).
# Después se recortas a que quede entre 0 y 1 (para que siga siendo una probabilidad válida).

# Más tarde, cada hogar puede percibir esa puntualidad con un ajuste (ej. rural percibe 5 p.p. menos).

tick_handler <- function(time, sim, agent) {      # Esta función se ejecutará una vez por cada unidad de tiempo (tick) en la simulación. El modelo ABM la volverá a llamar automáticamente en cada paso.
  # recibe tres cosas: time: el mes actual (0, 1, 2, …, 36), sim: el objeto de simulación (donde viven los agentes), agent: el programador interno del motor (para reagendar el siguiente tick).
  t <- as.integer(time)                                  # asegura que sea entero
  season_eff <- season_vec[(t %% 12) + 1]              # Estacionalidad según el mes (repite cada 12 ticks)
  policy_eff <- if (t >= 12 && t <= 15) 0.10 else 0    # Ventana de mejora (ej. meses 12–15 sube 10 p.p.)
  shock_eff <- rnorm(1, mean = 0, sd = 0.02)           # Ruido aleatorio (variación natural)
  base_pub <- 1 - delay_prob_base                      # Puntualidad pública del mes (en [0,1])
  p_public_tick <- max(0, min(1, base_pub + season_eff + policy_eff + shock_eff))
  
  for (i in 1:N) {                                # recorre los N hogares.
    ai <- getAgent(sim, i)                        # trae al agente i. Obtiene todos sus atributos guardados (estado, cred, edad, n_hijos, etc.).
    st <- getState(ai)                            # el nombre de una variable para guardar lo que devuelve getState(ai)
    
    # ai el agente que estoy manipulando
    # st los datos actuales de ese agente
    
    # puntualidad percibida por el hogar (usa el valor del mes y ajusta por zona - 5%)
    p_public <- if (st$zona == "rural") max(0, min(1, p_public_tick - 0.25)) else p_public_tick
    
    # ----- Cálculo del beneficio -----
    #       Beneficio por hijx si elegible
    hijos_cubiertos <- min(st$n_hijos, 2)     # Calcula cuántos hijxs reciben la transferencia (máximo 2).
    transfer_eff    <- if (st$elegible) 300 * hijos_cubiertos else 0   # Si el hogar es elegible, recibe 500 * hijos_cubiertos; si no, 0. Representa el monto mensual efectivo del apoyo.
    
    # ----- Cálculo del salario potencial del niñx -----
    #       salario potencial del niñx (modelo tipo Mincer)
    w_child <- st$salario
    
    # ----- Utilidades de las tres opciones -----
    # utilidades de tres opciones (E=solo escuela, ET=escuela y trabajo, T=solo trabajo)    
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
    # 💔 ELIMINADO: utilities <- c(E = U_E, ET = U_ET, T = U_T)  # junta las tres utilidades en un vector.
    # 💔 ELIMINADO: new_state <- names(which.max(utilities))     # elige el estado que tenga la utilidad más alta (la mejor decisión económica para ese hogar). which.max() da la posición; names() extrae E, ET o T
    
    #  ---------------  Idea intuitiva
    #                   Con which.max(utilities) siempre gana la utilidad más alta: todos los hogares actúan igual ante los mismos incentivos. El sistema se congela.
    #                   En la realidad hay gustos no observados, fricciones e información imperfecta.
    #                   Modelamos eso como un ruido aleatorio (tipo Gumbel). El resultado es que cada opción (E, ET, T) tiene una probabilidad de ser elegida.
    #                   Antes (argmax): which.max(utilities) siempre elige la opción con el número más grande. Resultado: todos hacen lo mismo y el modelo se congela.
    #                   Ahora (softmax): si una opción tiene un número más grande, tiene más chance, pero las otras no quedan en cero.
    #                   Resultado: hay probabilidades y, por lo tanto, movimiento entre E / ET / T.
    #          qué es tau (la temperatura):qué tanto permites salirse del óptimo:
    #          - tau chico:  casi siempre gana la mejor (se parece al argmax).
    #          - tau grande: más mezcla; las segundas mejores también se eligen a veces.
    
    
    utilities <- c(E = U_E, ET = U_ET, T = U_T) # utilidades determinísticas ya calculadas (juntas en un vector las tres utilidades que calculaste para este hogar en este mes)
    
    
    tau <- 200  # temperatura/margen de error/tolerancia al cambio:
    # se ajusta en pesos por ejemplo. Controla cuánta aleatoriedad permites:
    
    
    u_center <- utilities - max(utilities) # estabilidad numérica: restar la mayor utilidad antes de exp. 
    # Las utilidades pueden ser grandes (por ejemplo, 20000 pesos).
    # Al aplicar una exponencial (exp()), esos números se disparan.
    # Entonces, restar el máximo solo centra los valores:
    
    probs <- exp(u_center / tau)           # Convierte utilidades en pesos positivos con exp(…/tau).
    # toma cada utilidad centrada (u_center) y la transforma en un peso positivo usando la exponencial.
    # Por qué exponencial: garantiza que todos los pesos sean > 0 (nunca negativos). 
    # Además, una utilidad un poco mayor se vuelve mucho mayor tras exp, así que las opciones mejores reciben más peso.
    # Rol de tau: es la temperatura. Divide las utilidades para suavizar o acentuar diferencias:
    # - tau grande - > diferencias se achican - > pesos más parecidos.
    # - tau chico - > diferencias se agrandan -> el peso de la mejor domina.
    
    probs <- probs / sum(probs)            # convierte esos pesos en probabilidades dividiendo por su suma.
    # Resultado de ahora sum(probs) = 1, y cada probs[j] está entre 0 y 1 - > ya son probabilidades válidas.
    # La opción con mayor utilidad tenía el mayor peso exp, y al normalizar sigue con la probabilidad más alta, 
    # pero las demás no quedan en cero. 
    
    
    new_state <- sample(c("E","ET","T"), size = 1, prob = probs)  # elección según esas probabilidades
    
    
    # ----- Ver si el pago llegó a tiempo -----
    # NUEVO: Actualizar credibilidad solo si el hogar participa Y es elegible (observa su propio pago)
    if (new_state %in% c("E","ET") && st$elegible) {
      # Caso 1: El hogar participa y es elegible - > observa su propio pago
      # La puntualidad individual depende de la puntualidad pública del mes y un sorteo individual
      paid_on_time <- runif(1) < p_public
      cred_new <- (1 - lambda_cred) * st$cred + lambda_cred * as.numeric(paid_on_time)
    } else {
      # Caso 2: No participa o no es elegible - > no observa pago directo
      # Aprende de la señal pública del mes (p_public_tick), que representa el desempeño general del programa
      cred_new <- (1 - lambda_cred) * st$cred + lambda_cred * p_public_tick
    }
    
    # Si no participa o no es elegible, su credibilidad se actualiza según una señal pública promedio, 
    # Así, la credibilidad no colapsa a cero, sino que refleja la percepción colectiva del programa.
    
    # ----- Guardar nuevo estado y atributos -----
    #       Toma al agente que se esta actualizando (ai).
    #       Le guarda su nuevo estado y sus atributos en la simulación.
    
    setState(ai, list(
      new_state,
      theta   = st$theta,
      cred    = cred_new,
      n_hijos = st$n_hijos,
      Y_hogar = st$Y_hogar,
      elegible = st$elegible,
      edad = st$edad,
      grado = st$grado,
      salario = st$salario,
      zona = st$zona,
      sexo = st$sexo
    ))
  }
  
  if (time < Tmax) schedule(agent, newEvent(time + 1, tick_handler))
}


# =========================
# 5) Ejecutar
# =========================
sim$addLogger(newCounter("E", "E"))     # Solo escuela  1) columna "E"   2) cuenta agentes en estado "E"
sim$addLogger(newCounter("ET", "ET"))   # Escuela + trabajo
sim$addLogger(newCounter("T", "T"))     # Solo trabajo

# Los tres newCounter dan, en cada mes, cuántos hogares están en solo escuela (E), escuela+trabajo (ET) y solo trabajo (T), 
# y esa serie queda guardada en res para graficar o analizar.

schedule(sim$get, newEvent(0, tick_handler))  # esta línea arranca la simulación: es como presionar el botón play
res <- sim$run(0:Tmax)                        # sim$run() ejecuta todos los eventos que fueron programados
res$attend <- (res$E + res$ET) / N            # Crea una nueva columna en res llamada attend (asistencia): Proporción que estudia (solo escuela o escuela+trabajo)


res

# =========================
# 6) Graficar
# =========================
ggplot(res, aes(x = times, y = attend)) +
  geom_line(size = 1.3, color = "darkorange") +
  geom_area(alpha = 0.15, fill = "gold") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Asistencia simulada con base real de hogares",
       x = "Mes", y = "% hogares con niñxs en escuela") +
  theme_minimal(base_size = 14)
