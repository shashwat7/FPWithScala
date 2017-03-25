package Questions

import java.io.{File, FileWriter}
import java.security.Timestamp

import scala.util.Random

/**
  * Created by srastogi on 23-Mar-17.
  */
sealed trait Producer extends Runnable

class FileProducer(file: File) extends Producer{

  val MAX_FILE_SIZE = Long.MaxValue

  private def appendMessage(msg: String) = {
    val fw = new FileWriter(file, true)
    println("PRODUCER LOG - APPENDING MESSAGE: " + msg)
    fw.write(msg + "\n")
    fw.close()
    SimpleProducerConsumer.incrementCountOfLines
  }

  private def randomMessage: String = (System.currentTimeMillis() / 1000).toString

  override
  def run(): Unit = {

    while(true){
      file.synchronized{
        // Wait if file is full
        while(file.length() >= MAX_FILE_SIZE){
          println("PRODUCER LOG - FILE LENGTH: " + file.getTotalSpace)
          file.wait()
        }

        // Create and insert message
        val msg = randomMessage
        println("Produced Message - " + msg)
        appendMessage(msg)

        // Notifies consumer that it has produced something
        file.notify()

        // Sleep so that we display is better
        Thread.sleep(2000)
      }
    }

  }


}
