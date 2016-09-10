#1.-

v<-c(1,2,3,4,2,4,2,3);

original<-paste(v,collapse=",");

e<-2;

remplaza(v,e);

remplaza<-function(v,e){
  
  t<-length(v);
  
  for(i in 1:t){
    
    if(v[i]==e){
      
      v[i]=NA;
      
    }
    
  }
  
  datos<-paste(v,collapse=",");
  
  mensaje<-paste("En la secuencia",original,"ha sido reemplazado el carácter",e,"por NA, quedando la secuencia",datos,".");
  print(mensaje);
  
}

#2.-

v<-c(1,2,3,4);

e<-5;

existe(v,e);

existe<-function(v,e){
  
  t<-length(v);
  
  r<-FALSE;
  
  for(i in 1:t){
    
    if(v[i]==e){
      
      r<-TRUE;
      
      break;
      
    }
    
  }
  
  datos<-paste(v,collapse=",");
  
  mensaje<-paste("¿El número ",e," existe en el vector ",datos,"? ",r,".");
  print(mensaje);
  
}

#3.-

a<-1;
b<-2;
c<--8;

cuadratica(a,b,c);

cuadratica<-function(a,b,c){

s1<-0;
d<-(b^2)-(4*a*c);
r<--b+sqrt(d);
s1<-r/2*a;

s2<-0;
d<-(b^2)-(4*a*c);
r<--b-sqrt(d);
s2<-r/2*a;

mensaje<-paste("Para los números ",a,",",b,",",c,"los resultados de la ecuación cuadrática son ",s1,"y",s2,".");
print(mensaje);

}

#4.-

v<-c(1,1,1,1,2,3,4);

e<-1;

contar(v,e);

contar<-function(v,e){

t<-length(v);

r<-0;

for(i in 1:t){
  
  if(v[i]==e){
    
    r<-r+1;
    
  }
  
}

datos<-paste(v,collapse=",");

mensaje<-paste("Para el número ",e," existen ",r," coincidencias en el vector ",datos,".");
print(mensaje);

}

#5.- Versión 1.

v<-c(1,2,3,4);

estadistico(v);

estadistico<-function(v){

s<-sum(v);

t<-length(v);

p<-s/t;

r<-0;

for(i in 1:t){
  d<-(v[i]-p)^2;
  r<-r+d;
}
a<-r/(t-1);
#a<-r/t; //Desviación Estándar de la Población.

b<-sqrt(a);

print(b);

}

#5.- Versión 2.

v<-c(1,2,3,4);

estadistico(v);

estadistico<-function(v){
  
  m<-mean(v);
  
  s<-sd(v,na.rm=FALSE);
  
  datos<-paste(v,collapse=",");
  
  mensaje<-paste("Para el vector ",datos," la media es ",m," y la desviación estándar es ",s,".");
  print(mensaje);
  
}

#6.- Versión 1.
d<-9;

divisores(d);

divisores<-function(d){
  
  r<-0;
  
  for(i in 2:(d-1)){
    if(d%%i==0&&i!=d&&d-1!=1){
      r<-r+1;
    }
    
  }
  mensaje<-paste("La cantidad de divisores de ",d," son: ",r," números");
  print(mensaje);
  
}

#6.- Versión 2.
d<-2;

divisores(d);

divisores<-function(d){
  
  r<-0;
  
  for(i in 2:(d-1)){ #Revisar estructura de for.
    print(i);
    if(mod(d,i)==0&&i!=d&&d-1!=1){
      r<-r+1;
    }
    
  }
  mensaje<-paste("La cantidad de divisores de ",d," son: ",r," números");
  print(mensaje);
  
}

mod<-function(a,b){
    dif<-floor(a/b)
    return(a-dif*b)
    }