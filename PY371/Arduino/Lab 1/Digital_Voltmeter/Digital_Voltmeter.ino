int A = 8; //1nd bit
int B = 9; //2nd bit
int C = 10; //3rd bit
int D = 11; //4th bit
int W1 = 2;
int W2 = 3;
int W3 = 4;
int W4 = 5;
int voltage,one,ten,hundred,thousand,b;
int analog = 5;

void setup() {
  
  Serial.begin(9600);
  pinMode(A,OUTPUT);
  pinMode(B,OUTPUT);
  pinMode(C,OUTPUT);
  pinMode(D,OUTPUT);
  pinMode(W1,OUTPUT);
  pinMode(W2,OUTPUT);
  pinMode(W3,OUTPUT);
  pinMode(W4,OUTPUT);
  digitalWrite(W1,LOW);
  digitalWrite(W2,HIGH);
  digitalWrite(W3,HIGH);
  digitalWrite(W4,HIGH);
  voltage=0;
 
}

void loop() {
  // put your main code here, to run repeatedly:
  
  voltage = analogRead(5);
  Serial.println(voltage);
  
  one=voltage%10;
  ten=(voltage%100)/10;
  hundred=(voltage%1000)/100;
  thousand=(voltage%10000)/1000;
  
  digitalWrite(W1,LOW);
  
  for (b=0;b<4;b++)
    {
     if ( one & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
  
  digitalWrite(W1,HIGH);
  digitalWrite(W2,LOW);
  
  for (b=0;b<4;b++)
    {
     if ( ten & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
  
  digitalWrite(W2,HIGH);
  digitalWrite(W3,LOW);
  
  for (b=0;b<4;b++)
    {
     if ( hundred & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
    
  digitalWrite(W3,HIGH);
  digitalWrite(W4,LOW);
  
   for (b=0;b<4;b++)
    {
     if ( thousand & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
  
  digitalWrite(W4,HIGH);
  

  //digitalWrite(A,HIGH);
  //digitalWrite(B,HIGH);
  //digitalWrite(C,HIGH);
  //digitalWrite(D,HIGH);
}
