int A = 8; //1nd bit
int B = 9; //2nd bit
int C = 10; //3rd bit
int D = 11; //4th bit
int W1 = 2;
int W2 = 3;
int W3 = 4;
int W4 = 5;
int count,one,ten,hundred,thousand,b;

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
  count=0;
 
}

void loop() {
  // put your main code here, to run repeatedly:
  
  one=count%10;
  ten=(count%100)/10;
  hundred=(count%1000)/100;
  thousand=(count%10000)/1000;
  
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
  
  Serial.println(count);   
  //digitalWrite(A,HIGH);
  //digitalWrite(B,HIGH);
  //digitalWrite(C,HIGH);
  //digitalWrite(D,HIGH);
  
  count++;
  
  delay(10);
  
  if(count==9999)
    delay(1000000);
}
