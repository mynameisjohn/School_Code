int data[100];
int val=0,analogPin=0;
int counter=0;

void setup()
{
  Serial.begin(9600);
}

void loop()
{
  if (Serial.read()!=-1){
  read();
  write();}
}

void read()
{
  for (int i=0;i<100;i++){
    data[i]=analogRead(analogPin);
    delay(10);}
}

void write()
{
  for (int i=99;i>=0;i--)
    Serial.println(data[i]);
}
