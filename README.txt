# -in-R


6. 기타 (R 가지확률 분석 모형)

# 기본 변수 (마르코프에서는 인구수, 의사결정모형에서는 개별 노드의 확률로 보면 됨)
DM1_FSL_70mg_가지확률 <- 0
DM1_FSL_45mg_가지확률 <- 0
DM1_FSL_keto_가지확률 <- 0
DM1_FSL_noevent_가지확률 <- 0

DM1_SBGM_70mg_가지확률 <- 0
DM1_SBGM_45mg_가지확률 <- 0
DM1_SBGM_keto_가지확률 <- 0
DM1_SBGM_noevent_가지확률 <- 0

DM2_FSL_70mg_가지확률 <- 0
DM2_FSL_45mg_가지확률 <- 0
DM2_FSL_keto_가지확률 <- 0
DM2_FSL_noevent_가지확률 <- 0

DM2_SBGM_70mg_가지확률 <- 0
DM2_SBGM_45mg_가지확률 <- 0
DM2_SBGM_keto_가지확률 <- 0
DM2_SBGM_noevent_가지확률 <- 0

# 입력값 

cycles = 1
코호트 = 1   #의사결정모형이므로 코호트를 1로 정의해야 개별 노드의 확률이 나옴)

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
                       0.1, 1, 0, 0, 0,
                       0.2, 0, 1 ,0, 0,
                       0.4, 0, 0, 1, 0,
                       0.5, 0, 0, 0, 1)
colnames(DM1_FSL_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')
rownames(DM1_FSL_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')

DM1_SBGM_matrix <- matrix(ncol=5, nrow = 5)
DM1_FSL_matrix[,] <- c(0, 0, 0, 0, 0,
                       0.2, 1, 0, 0, 0,
                       0.3, 0, 1 ,0, 0,
                       0.2, 0, 0, 1, 0,
                       0.3, 0, 0, 0, 1)
colnames(DM1_SBGM_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')
rownames(DM1_SBGM_matrix) <- c('start', '70mg', '45mg', 'keto', 'noevent')



# 주기별 사람수(가지확률) 구하기
DM1_FSL_70mg_가지확률 <- 코호트 * DM1_FSL_matrix[1,2]
DM1_FSL_45mg_가지확률 <- 코호트 * DM1_FSL_matrix[1,3]
DM1_FSL_keto_가지확률 <- 코호트 * DM1_FSL_matrix[1,4]
DM1_FSL_noevent_가지확률 <- 코호트 * DM1_FSL_matrix[1,5]


# DM1 - 주기별 비용 구하기

DM1_FSL_70mg_총비용 <- 0
DM1_SBGM_70mg_총비용 <- 0
DM1_총비용 <- 0

DM1_FSL_70mg_총비용 <- DM1_FSL_70mg_가지확률 * DM1_FSL_70mg_비용 
DM1_SBGM_70mg_총비용 <- DM1_SBGM_70mg_가지확률 * DM1_SBGM_70mg_비용


DM1_총비용 <- DM1_FSL_70mg_총비용 + DM1_SBGM_70mg_총비용


# DM1 - 주기별 효용 구하기  

DM1_FSL_70mg_총효용 <- 0
DM1_SBGM_70mg_총효용 <- 0
DM1_총효용 <- 0

DM1_FSL_70mg_총효용 <- DM1_FSL_70mg_가지확률 * DM1_FSL_70mg_효용 
DM1_SBGM_70mg_총효용 <- DM1_SBGM_70mg_가지확률 * DM1_SBGM_70mg_효용

DM1_총효용 <- DM1_FSL_70mg_총효용 + DM1_SBGM_70mg_총효용



    
    
    #--------------- PSA test
    DM1_FSL_비용=rnorm(100, mean = 10000, sd = 1000)
    DM1_FSL_QALY=rnorm(100, mean = 10, sd = 1)
    DM1_FSL_table <- data.frame(DM1_FSL_비용, DM1_FSL_QALY)

    ggplot(DM1_FSL_table, aes(x=DM1_FSL_QALY, y=DM1_FSL_비용)) +
      geom_point() + 
      xlim(1,20) +
      ylim(1,15000) +
      geom_abline(slope=1500, color='red')
    
    
#

WTP500 = 500
WTP1000 = 1000
WTP1500 = 1500
WTP2000 = 2000

    
    
for(i in 1:100){

  if(DM1_FSL_table$DM1_FSL_QALY[i] * WTP500 > DM1_FSL_table$DM1_FSL_비용[i]){
    DM1_FSL_table$test_WTP500[i] = 'pass'
  }
  else{
    DM1_FSL_table$test_WTP500[i] = 'fail'
  }
}      

for(i in 1:100){
  
  if(DM1_FSL_table$DM1_FSL_QALY[i] * WTP1000 > DM1_FSL_table$DM1_FSL_비용[i]){
    DM1_FSL_table$test_WTP1000[i] = 'pass'
  }
  else{
    DM1_FSL_table$test_WTP1000[i] = 'fail'
  }
}                         

for(i in 1:100){
  
  if(DM1_FSL_table$DM1_FSL_QALY[i] * WTP1500 > DM1_FSL_table$DM1_FSL_비용[i]){
    DM1_FSL_table$test_WTP1500[i] = 'pass'
  }
  else{
    DM1_FSL_table$test_WTP1500[i] = 'fail'
  }
}      

for(i in 1:100){
  
  if(DM1_FSL_table$DM1_FSL_QALY[i] * WTP2000 > DM1_FSL_table$DM1_FSL_비용[i]){
    DM1_FSL_table$test_WTP2000[i] = 'pass'
  }
  else{
    DM1_FSL_table$test_WTP2000[i] = 'fail'
  }
}      
                                                
DM1_FSL_table %>% group_by(test_WTP500)
  summarise(percent = n(fail))

DM1_FSL_table %>% summarise(percent_pass_WTP500 = )

