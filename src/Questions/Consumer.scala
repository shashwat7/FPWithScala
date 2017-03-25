package Questions

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.Files

/**
  * Created by srastogi on 23-Mar-17.
  */
sealed trait Consumer extends Runnable

class FileConsumer(file: File) extends Consumer{

  var POSITION: Int = 0
  val reader = new BufferedReader(new FileReader(file))

  def readLine = {
    val line = reader.readLine()
    if(line == null || line.trim.isEmpty == "") null
    else{
      POSITION = POSITION + 1
      line
    }
  }

  def close = reader.close()

  override def run(): Unit = {

    while(true){
      file.synchronized{
        // wait consumer if it had read everything
        while(POSITION >= SimpleProducerConsumer.getCountOfLines){
          println("CONSUMER LOG - FILE POSITION: " + POSITION + ". COUNT OF LINES: " + SimpleProducerConsumer.getCountOfLines)
          file.wait()
        }

        // retrieve next line
        val line = readLine
        println("Consumed Message - " + line)

        // Wake up the producer thread
        file.notify()

        // Make it sleep for clearer output
        Thread.sleep(1000)
      }
    }
  }
}
