#Pobreza - prospectivas
set.seed(1)

run_model <- function(N=5000, Tmax=36,
                      delay_prob_base=0.30, lambda_cred=0.30,
                      base_transfer_per_child=300,
                      max_children_paid=99,
                      poverty_line=4500) {
  
  # ---- Población sintética
  theta  <- rnorm(N, mean=300, sd=80)
  cred   <- rep(1 - delay_prob_base, N)
  n_hijos <- sample(0:3, N, replace=TRUE, prob=c(0.25,0.40,0.25,0.10))
  Y_hogar <- rlnorm(N, meanlog=log(4000), sdlog=0.5)
  elegible <- Y_hogar < poverty_line
  state <- rep("N", N); state[seq_len(min(10, N))] <- "A"
  
  # ---- Serie
  A <- integer(Tmax+1); Nn <- integer(Tmax+1)
  A[1] <- sum(state=="A"); Nn[1] <- sum(state=="N")
  
  for (t in 1:Tmax) {
    hijos_cubiertos <- pmin(n_hijos, max_children_paid)
    transfer_eff <- ifelse(elegible, base_transfer_per_child * hijos_cubiertos, 0)
    benefit <- transfer_eff * cred
    new_state <- ifelse(benefit >= theta, "A", "N")
    
    paid_on_time <- (new_state=="A") & (runif(N) > delay_prob_base)
    cred <- (1 - lambda_cred)*cred + lambda_cred*as.numeric(paid_on_time)
    
    state <- new_state
    A[t+1]  <- sum(state=="A")
    Nn[t+1] <- N - A[t+1]
  }
  
  data.frame(times=0:Tmax, A=A, N=Nn, attend=A/N,
             N=N, delay_prob_base=delay_prob_base,
             lambda_cred=lambda_cred,
             base_transfer_per_child=base_transfer_per_child,
             max_children_paid=max_children_paid,
             poverty_line=poverty_line)
}

# ===== 1) Caso base
base <- run_model(max_children_paid=99, poverty_line=4500)
ggplot(base, aes(times, attend)) + geom_line() +
  coord_cartesian(ylim=c(0,1)) + theme_minimal() +
  labs(title="Asistencia - Caso base", x="Mes", y="% que asiste")

# ===== 2) Tope = 2 (tamaño de familia)
cap2 <- run_model(max_children_paid=2, poverty_line=4500)
cmp12 <- rbind(transform(base, esc="Base"), transform(cap2, esc="Cap2"))
ggplot(cmp12, aes(times, attend, color=esc)) + geom_line() +
  coord_cartesian(ylim=c(0,1)) + theme_minimal() +
  labs(title="Asistencia: sin tope vs tope 2", x="Mes", y="% asiste")

# ===== 3) Línea de pobreza
pl3500 <- run_model(poverty_line = 3500)
pl4500 <- run_model(poverty_line = 4500)   # <- NO uses 'base' aquí
pl5500 <- run_model(poverty_line = 5500)

cmp_pl <- rbind(
  transform(pl3500, esc = "PL3500"),
  transform(pl4500, esc = "PL4500"),
  transform(pl5500, esc = "PL5500")
)

stopifnot(nrow(cmp_pl) > 0)
stopifnot(all(c("times","attend","esc") %in% names(cmp_pl)))

p <- ggplot(cmp_pl, aes(x = times, y = attend, color = esc)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Asistencia por linea de pobreza", x = "Mes", y = "% asiste", color = "Escenario") +
  theme_minimal(base_size = 14)

print(p)
ggsave("03_linea_pobreza_series.png", p, width = 8, height = 5, dpi = 300)


# ===== 4) Sensibilidad: monto, elegibilidad, puntualidad
A_base <- tail(base$attend,1)
A_m200 <- tail(run_model(base_transfer_per_child=200)$attend,1)
A_m400 <- tail(run_model(base_transfer_per_child=400)$attend,1)
A_e3500 <- tail(pl3500$attend,1)
A_e5500 <- tail(pl5500$attend,1)
A_p20 <- tail(run_model(delay_prob_base=0.20)$attend,1)
A_p50 <- tail(run_model(delay_prob_base=0.50)$attend,1)

sens <- data.frame(
  factor=c("Monto","Monto","Monto","Elegibilidad","Elegibilidad","Elegibilidad","Puntualidad","Puntualidad","Puntualidad"),
  level=c("200","300","400","PL3500","PL4500","PL5500","Retraso20","Retraso30","Retraso50"),
  attend=c(A_m200, A_base, A_m400, A_e3500, A_base, A_e5500, A_p20, A_base, A_p50)
)
ggplot(sens, aes(level, attend, group=factor, color=factor)) +
  geom_line() + geom_point() + coord_cartesian(ylim=c(0,1)) +
  theme_minimal() + labs(title="Sensibilidad (asistencia final)", x="Nivel", y="% asiste")
