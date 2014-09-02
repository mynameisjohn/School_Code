int counter = 0;

void setup()
{
   Serial.begin(9600); 
   Serial.println("Hello World!");
}

void loop()
{
  Serial.print("Seconds since start: ");
  Serial.println(counter);
  counter++;
}
