# 입력값

cycles = 1 # 주기 = 가로 기준으로 확률노드의 개수 // 마지막 주기에서의 값을 최종 가지확률로 보면 됨
코호트 = 1 # 의사결정모형은 마르코프와 달리 1명이 특정 노드까지 갈 확률을 구하는 것이므로 코호트를 1로 입력한다.

DM1_FSL_70mg_가지확률 <- 0.3
DM1_FSL_45mg_가지확률 <- 0.2
DM1_FSL_keto_가지확률 <- 0.3
DM1_FSL_noevent_가지확률 <- 0.2

DM1_SBGM_70mg_가지확률 <- 0.4
DM1_SBGM_45mg_가지확률 <- 0.3
DM1_SBGM_keto_가지확률 <- 0.2
DM1_SBGM_noevent_가지확률 <- 0.1


DM1_FSL_70mg_비용 <- 20
DM1_FSL_45mg_비용 <- 10
DM1_FSL_keto_비용 <- 5
DM1_FSL_noevent_비용 <- 2

DM1_SBGM_70mg_비용 <- 30
DM1_SBGM_45mg_비용 <- 20
DM1_SBGM_keto_비용 <- 10
DM1_SBGM_noevent_비용 <- 5

DM1_FSL_70mg_효용 <- 0.3
DM1_FSL_45mg_효용 <- 0.6
DM1_FSL_keto_효용 <- 0.8
DM1_FSL_noevent_효용 <- 0.9

DM1_SBGM_70mg_효용 <- 0.2
DM1_SBGM_45mg_효용 <- 0.4
DM1_SBGM_keto_효용 <- 0.7
DM1_SBGM_noevent_효용 <- 0.8




DM2_FSL_70mg_가지확률 <- 0
DM2_FSL_45mg_가지확률 <- 0
DM2_FSL_keto_가지확률 <- 0
DM2_FSL_noevent_가지확률 <- 0

DM2_SBGM_70mg_가지확률 <- 0
DM2_SBGM_45mg_가지확률 <- 0
DM2_SBGM_keto_가지확률 <- 0
DM2_SBGM_noevent_가지확률 <- 0


DM2_FSL_70mg_비용 <- 0
DM2_FSL_45mg_비용 <- 0
DM2_FSL_keto_비용 <- 0
DM2_FSL_noevent_비용 <- 0

DM2_SBGM_70mg_비용 <- 0
DM2_SBGM_45mg_비용 <- 0
DM2_SBGM_keto_비용 <- 0
DM2_SBGM_noevent_비용 <- 0

DM2_FSL_70mg_효용 <- 0
DM2_FSL_45mg_효용 <- 0
DM2_FSL_keto_효용 <- 0
DM2_FSL_noevent_효용 <- 0

DM2_SBGM_70mg_효용 <- 0
DM2_SBGM_45mg_효용 <- 0
DM2_SBGM_keto_효용 <- 0
DM2_SBGM_noevent_효용 <- 0

# 가지확률 행렬

DM1_FSL_matrix <- matrix(ncol=5, nrow = 5)


DM1_FSL_matrix[,] <- c(0, 0, 0, 0, 0,
                       DM1_FSL_70mg_가지확률, 1 , 0, 0, 0,
                       DM1_FSL_45mg_가지확률, 0, 1 ,0, 0,
                       DM1_FSL_keto_가지확률, 0, 0, 1, 0,
                       DM1_FSL_noevent_가지확률, 0, 0, 0, 1)
colnames(DM1_FSL_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')
rownames(DM1_FSL_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')

DM1_SBGM_matrix <- matrix(ncol=5, nrow = 5)
DM1_SBGM_matrix[,] <- c(0, 0, 0, 0, 0,
                      DM1_SBGM_70mg_가지확률, 1, 0, 0, 0,
                      DM1_SBGM_45mg_가지확률, 0, 1 ,0, 0,
                      DM1_SBGM_keto_가지확률, 0, 0, 1, 0,
                      DM1_SBGM_noevent_가지확률, 0, 0, 0, 1)
colnames(DM1_SBGM_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')
rownames(DM1_SBGM_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')



# DM1 - 총 비용 구하기


DM1_FSL_총비용 <- 0
DM1_SBGM_총비용 <- 0

DM1_FSL_총비용 <- DM1_FSL_matrix[1,2] * DM1_FSL_70mg_비용 +
  DM1_FSL_matrix[1,3] * DM1_FSL_45mg_비용 +
  DM1_FSL_matrix[1,4] * DM1_FSL_keto_비용 +
  DM1_FSL_matrix[1,5] * DM1_FSL_noevent_비용
  
DM1_SBGM_총비용 <- DM1_SBGM_matrix[1,2] * DM1_SBGM_70mg_비용 +
  DM1_SBGM_matrix[1,3] * DM1_SBGM_45mg_비용 +
  DM1_SBGM_matrix[1,4] * DM1_SBGM_keto_비용 +
  DM1_SBGM_matrix[1,5] * DM1_SBGM_noevent_비용


# DM1 - 주기별 효용 구하기  

DM1_FSL_QALY <- 0
DM1_SBGM_QALY <- 0

DM1_FSL_QALY <- DM1_FSL_matrix[1,2] * DM1_FSL_70mg_효용 +
  DM1_FSL_matrix[1,3] * DM1_FSL_45mg_효용 +
  DM1_FSL_matrix[1,4] * DM1_FSL_keto_효용 +
  DM1_FSL_matrix[1,5] * DM1_FSL_noevent_효용

DM1_SBGM_QALY <- DM1_SBGM_matrix[1,2] * DM1_SBGM_70mg_효용 +
  DM1_SBGM_matrix[1,3] * DM1_SBGM_45mg_효용 +
  DM1_SBGM_matrix[1,4] * DM1_SBGM_keto_효용 +
  DM1_SBGM_matrix[1,5] * DM1_SBGM_noevent_효용


# ICER & ICER 시각화

DM1_FSL_ICER <- DM1_FSL_총비용 / DM1_FSL_QALY
DM1_SBGM_ICER <- DM1_SBGM_총비용 / DM1_SBGM_QALY

DM1_ICER <- data.frame(x = c(DM1_FSL_QALY,DM1_SBGM_QALY), y = c(DM1_FSL_총비용,DM1_SBGM_총비용))
colnames(DM1_ICER) <- c('inc_QALY', 'inc_cost')
rownames(DM1_ICER) <- c('DM1_FSL','DM1_SBGM')


ggplot(DM1_ICER, aes(x=inc_QALY, y = inc_cost)) +
  geom_point() +
  geom_abline(slope=10, intercept = 0) +
  xlim(0,30) +
  ylim(0,30)


# 확률적 민감도 분석 - 입력값

n = 1000
for_n = 1:n

psa_DM1_FSL_70mg_가지확률 <- rnorm(n, mean=DM1_FSL_70mg_가지확률, sd=0.3)
psa_DM1_FSL_45mg_가지확률 <- rnorm(n, mean=DM1_FSL_45mg_가지확률, sd=0.3)
psa_DM1_FSL_keto_가지확률 <- rnorm(n, mean=DM1_FSL_keto_가지확률, sd=0.3)
psa_DM1_FSL_noevent_가지확률 <- 1 - (psa_DM1_FSL_70mg_가지확률+psa_DM1_FSL_45mg_가지확률+psa_DM1_FSL_keto_가지확률)

psa_DM1_SBGM_70mg_가지확률 <- rnorm(n, mean=DM1_SBGM_70mg_가지확률, sd=0.3)
psa_DM1_SBGM_45mg_가지확률 <- rnorm(n, mean=DM1_SBGM_45mg_가지확률, sd=0.3)
psa_DM1_SBGM_keto_가지확률 <- rnorm(n, mean=DM1_SBGM_keto_가지확률, sd=0.3)
psa_DM1_SBGM_noevent_가지확률 <- 1 - (psa_DM1_SBGM_70mg_가지확률+psa_DM1_SBGM_45mg_가지확률+psa_DM1_SBGM_keto_가지확률)

psa_DM1_FSL_70mg_비용 <- rnorm(n, mean=DM1_FSL_70mg_비용, sd=1)
psa_DM1_FSL_45mg_비용 <- rnorm(n, mean=DM1_FSL_45mg_비용, sd=1)
psa_DM1_FSL_keto_비용 <- rnorm(n, mean=DM1_FSL_keto_비용, sd=1)
psa_DM1_FSL_noevent_비용 <- rnorm(n, mean=DM1_FSL_noevent_비용, sd=1)

psa_DM1_SBGM_70mg_비용 <- rnorm(n, mean=DM1_SBGM_70mg_비용, sd=1)
psa_DM1_SBGM_45mg_비용 <- rnorm(n, mean=DM1_SBGM_45mg_비용, sd=1)
psa_DM1_SBGM_keto_비용 <- rnorm(n, mean=DM1_SBGM_keto_비용, sd=1)
psa_DM1_SBGM_noevent_비용 <- rnorm(n, mean=DM1_SBGM_noevent_비용, sd=1)

psa_DM1_FSL_70mg_효용 <- rnorm(n, mean=DM1_FSL_70mg_효용, sd=1)
psa_DM1_FSL_45mg_효용 <- rnorm(n, mean=DM1_FSL_45mg_효용, sd=1)
psa_DM1_FSL_keto_효용 <- rnorm(n, mean=DM1_FSL_keto_효용, sd=1)
psa_DM1_FSL_noevent_효용 <- rnorm(n, mean=DM1_FSL_noevent_효용, sd=1)

psa_DM1_SBGM_70mg_효용 <- rnorm(n, mean=DM1_SBGM_70mg_효용, sd=1)
psa_DM1_SBGM_45mg_효용 <- rnorm(n, mean=DM1_SBGM_45mg_효용, sd=1)
psa_DM1_SBGM_keto_효용 <- rnorm(n, mean=DM1_SBGM_keto_효용, sd=1)
psa_DM1_SBGM_noevent_효용 <- rnorm(n, mean=DM1_SBGM_noevent_효용, sd=1)



psa_DM1_FSL_총비용 <- psa_DM1_FSL_70mg_가지확률 * psa_DM1_FSL_70mg_비용 +
  psa_DM1_FSL_45mg_가지확률 * psa_DM1_FSL_45mg_비용 +
  psa_DM1_FSL_keto_가지확률 * psa_DM1_FSL_keto_비용 +
  psa_DM1_FSL_noevent_가지확률 * psa_DM1_FSL_noevent_비용

psa_DM1_FSL_QALY <- psa_DM1_FSL_70mg_가지확률 * psa_DM1_FSL_70mg_효용 +
  psa_DM1_FSL_45mg_가지확률 * psa_DM1_FSL_45mg_효용 +
  psa_DM1_FSL_keto_가지확률 * psa_DM1_FSL_keto_효용 +
  psa_DM1_FSL_noevent_가지확률 * psa_DM1_FSL_noevent_효용

psa_DM1_SBGM_총비용 <- psa_DM1_SBGM_70mg_가지확률 * psa_DM1_SBGM_70mg_비용 +
  psa_DM1_SBGM_45mg_가지확률 * psa_DM1_SBGM_45mg_비용 +
  psa_DM1_SBGM_keto_가지확률 * psa_DM1_SBGM_keto_비용 +
  psa_DM1_SBGM_noevent_가지확률 * psa_DM1_SBGM_noevent_비용

psa_DM1_SBGM_QALY <- psa_DM1_SBGM_70mg_가지확률 * psa_DM1_SBGM_70mg_효용 +
  psa_DM1_SBGM_45mg_가지확률 * psa_DM1_SBGM_45mg_효용 +
  psa_DM1_SBGM_keto_가지확률 * psa_DM1_SBGM_keto_효용 +
  psa_DM1_SBGM_noevent_가지확률 * psa_DM1_SBGM_noevent_효용

psa_DM1_inc_cost = psa_DM1_FSL_총비용 - psa_DM1_SBGM_총비용
psa_DM1_inc_QALY = psa_DM1_FSL_QALY - psa_DM1_SBGM_QALY


psa_DM1_table <- data.frame(psa_DM1_inc_cost, psa_DM1_inc_QALY)

# PSA 실행

WTP = 3

psa_DM1_WTP3 <- 0


for(i in for_n){
if(psa_DM1_inc_QALY[i] * WTP < psa_DM1_inc_cost[i]){
  psa_DM1_WTP3[i] <- 'Fail'
}
else{
  psa_DM1_WTP3[i] <- 'Pass'
}
}


psa_DM1_table <- data.frame(psa_DM1_inc_cost, psa_DM1_inc_QALY, psa_DM1_WTP3)


ggplot(psa_DM1_table, aes(x=psa_DM1_inc_QALY, y=psa_DM1_inc_cost, color=psa_DM1_WTP3)) +
  geom_point() +
  geom_abline(slope=3)




help(rbeta)

