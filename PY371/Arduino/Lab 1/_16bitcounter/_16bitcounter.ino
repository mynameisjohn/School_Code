int A = 8; //1nd bit
int B = 9; //2nd bit
int C = 10; //3rd bit
int D = 11; //4th bit
int W = 5;
int count;

void setup() {
  pinMode(A,OUTPUT);
  pinMode(B,OUTPUT);
  pinMode(C,OUTPUT);
  pinMode(D,OUTPUT);
  pinMode(W,OUTPUT);
  digitalWrite(W,LOW);
  count=0;
 
}

void loop() {
  // put your main code here, to run repeatedly:
  
  for (int b=0;b<4;b++)
    {
     if ( count & (1 << b))
       digitalWrite(b+8,HIGH);
     else
       digitalWrite(b+8,LOW);
    }
   
  //digitalWrite(A,HIGH);
  //digitalWrite(B,HIGH);
  //digitalWrite(C,HIGH);
  //digitalWrite(D,HIGH);
  
  count++;
  if (count>9)
    count=0;
  delay(1000);
}
