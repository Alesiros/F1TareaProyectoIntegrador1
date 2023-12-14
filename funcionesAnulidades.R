#Vencidas

#Valor Futuro
VFVen=function(pago,tasaDelPeriodo,periodos){
  r=tasaDelPeriodo
  VF=pago*(((((1+r)^periodos))-1)/r)
  print(
    paste0("El valor final en el periodo ", periodos, " con una tasa en el periodo del ", r, " y con pagos de $",pago," es: $",VF )
  )
}
pagoVFVen=function(VF,tasaDelPeriodo,periodos){
  r=tasaDelPeriodo
  A=VF/((((1+r)^periodos)-1)/r)
  print(
    paste0("El valor de los pagos para llegar a tener $",VF," en ",periodos," periodos, con una tasa del ",r," es de: $",A )
  )
}
periodosVFVen=function(pago,VF,tasaDelPeriodo){
  r=tasaDelPeriodo
  p1=(((VF*r)/pago)+1)
  p2=(1+r)
  t= (log(p1,10))/(log(p2,10))
  print(
    paste0("El número de periodos para tener $",VF,", con pagos de $",pago," y una tasa periodica de ",r," es de ",t)
)}
tasaVFVen=function(pago,VF,periodos){  
  r=0.0000001
  A=pago
  t=periodos
  iz=VF/A
  der=(((1+r)^t)-1)/r
  dif=iz-der
    while(dif>0){
      r=r*1.0000001
      izq=VF/A
      der=(((1+r)^t)-1)/r
      dif=iz-der
    }
  a=r/10000
  print(
    paste0("La aproximación de la tasa por periodo es de ",a)
  )
}

#Valor Actual
VAVen=function(pago,tasaDelPeriodo,periodos){
  r=tasaDelPeriodo
  A=pago
  VA=(A)*(((1-((1+r)^(-periodos)))/r))
  print(
    paste0("El valor actual en el periodo ",periodos,", con pagos de $",pago," y una tasa del ",r," es de: $",VA)
  )
}
pagoVAVen=function(VA,tasaDelPeriodo,periodos){
  r=tasaDelPeriodo
  t=periodos
  A=(VA)/(((1-((1+r))^-(t)))/r)
  print(
    paste0("El valor de los pagos es de $",A)
  )
}
periodosVAVen=function(VA,pago,tasaDelPeriodo){
  A=pago
  r=tasaDelPeriodo
  p1=-log((1-((VA*r)/A)),10)
  p2=log((1+r),10)
  t=p1/p2
  print(
    paste0("Los periodos totales por $",VA," en pagos de $",A," y una tasa del ",r," son ",t)
  )
}
tasaVAVen=function(VA,pago,periodos){
  r=0.0001
  A=pago
  t=periodos
  izq=VA/A
  der=(((1-((1+r)^(-t))))/r)
  dif=izq-der
  while(dif<0){
    r=r*1.0001
    izq=VA/A
    der=((1-((1+r)^(-t)))/r)
    dif=izq-der
  }
  print(
    paste0("La tasa aproximada es de ",r)
  )
}

#Anticipadas

#Valor Futuro
VFAnt=function(pago,tasaDelPeriodo,periodos){
  A=pago
  r=tasaDelPeriodo
  t=periodos
  VF=A*(1+r)*((((1+r)^t)-1)/r)
 print(
   paste0("El Valor Final es de $",VF)
 ) 
}
pagoVFAnt=function(VF,tasaDelPeriodo,periodos){
  r=tasaDelPeriodo
  t=periodos
  A=((VF*r)/((((1+r)^t)-1)*(1+r)))
  print(
    paste0("El valor de los pagos que se necesitan en es de $",A)
  )
}
periodosVFAnt=function(VF,pago,tasadelperiodo){
  A=pago
  r=tasadelperiodo
  p1=log((((VF*r)/((A*(1+r))))+1),10)
  p2=log((1+r),10)
  t=p1/p2
  print(
    paste0("El numero de periodos que se necesitan son de ",t," periodos")
  )
}
tasaVFAnt=function(VF,pago,periodos){
  A=pago
  t=periodos
  r=0.0001
  izq=VF/(A*(1+r))
  der=(((1+r)^t)-1)/r
  dif=izq-der
  while(dif>0){
    r=r*1.0001
    izq=VF/(A*(1+r))
    der=(((1+r)^t)-1)/r
    dif=izq-der
  
  }
  print(
    paste0("EL valor de la tasa estimada es de: ",r)
  )
}

#valor Actual
VAAnt=function(pago,tasaDelPeriodo,periodo){
  A=pago
  r=tasaDelPeriodo
  t=periodo
  VA=A*((1-((1+r)^(-t)))/r)*(1+r)
  print(paste0("El Valor actual es: $",VA))
}
pagoVAAnt=function(VA,tasaDelPeriodo,periodo){
  r=tasaDelPeriodo
  t=periodo
  A=(VA/(1+r))/((1-((1+r)^(-t)))/r)
     print(
       paste0("El valor de los pagos es de $",A)
     )
}
periodosVAAnt=function(VA,pago,tasaDelPeriodo){
  r=tasaDelPeriodo
  A=pago
  p1=-log((1-((VA*r)/((1+r)*A))),10)
  p2=log((1+r),10)
  t=p1/p2
  print(
    paste0("El número necesarios de periodos es de ",t," periodos")
  )
}
tasaVAAnt=function(VA,pago,periodos){
  A=pago
  t=periodos
  r=0.000001
  p1=(VA)/(A*(1+r))
  p2=((1-((1+r)^-t))/r)
  dif=p1-p2
  while(dif<0){
    r=r*(1.001)
    p1=(VA)/(A*(1+r))
    p2=((1-((1+r)^-t))/r)
    dif=p1-p2
  }
  print(
    paste0("La aproximación de la tasa es de :",r)
  )
}

#Diferidas
VADif=function(pago,tasaDePeriodo, periodos,diferimiento){
  A=pago
  r=tasaDePeriodo
  t=periodos
  d=diferimiento
  VP=A*((1-((1+r)^(-t)))/r)
  VA=VP/((1+r)^d)
  print(
    paste0("El valor actual es de $",VA)
  )
}
pagoVADif=function(VA,tasaDelPeriodo,periodos,diferimiento){
  d=diferimiento
  r=tasaDelPeriodo
  t=periodos
  VA1=VA*((1+r)^d)
  A=(VA1/((((1-((1+r))^-(t)))/r)))
  print(
    paste0("Los pagos de la deuda son de $",A)
  )
}

