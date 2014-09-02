int counter;

void setup()
{
  Serial.begin(38400);
  counter=0;
}

void loop()
{
  Serial.println(counter);
  counter++;
  if (counter>360)
  counter=0;
}
