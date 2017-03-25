package Questions

import java.io.{BufferedReader, File, FileReader}

/**
  * Created by srastogi on 23-Mar-17.
  */



object SimpleProducerConsumer {

  var countOfLines = 0

  def getCountOfLines = countOfLines
  def incrementCountOfLines = countOfLines = countOfLines + 1
  def setCountOfLines(n: Int) = countOfLines = n

  def main(args: Array[String]): Unit = {
    val TEMP_FILE = new File("temp_pc.txt")
    if(!TEMP_FILE.exists()) TEMP_FILE.createNewFile()
    else {
      TEMP_FILE.delete()
      TEMP_FILE.createNewFile()
    }
    setCountOfLines(0)

    val producerThread = new Thread(new FileProducer(TEMP_FILE))
    val consumerThread = new Thread(new FileConsumer(TEMP_FILE))

    producerThread.start()
    consumerThread.start()

    TEMP_FILE.delete()
  }

}
