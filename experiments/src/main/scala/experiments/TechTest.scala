package experiments

import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._

case class Block(block_number: Int, transaction_count: Int, timestamp: Int)

case class Transaction(block_number: Int, sender_address: String, recipient_address: String, transaction_id: String, value: Double)

case class FiatExchange(block_number: Int, exchange_rate: Double)

case class TransactionWithFullInfo(sender_address: String, recipient_address: String, transaction_id: String, fiat_value: Double, timestamp: Int)

case class Groups(bks: List[Block], txs: List[Transaction], fxs: List[FiatExchange])

object TechTest extends App {
  val inputs = List.fill(100000)(List(
    "tx:0,0x6887d050357a1f466637cac4f8571d86ffbd9372,0x6887d050357a1f466637cac4f8571d86ffbd9372,0xf7ea3775e7da121407b24373e029856e4993d5ea680a72aa3ec3db086cfff915,1.8640690768236101",
    "tx:0,0xcf0c6e6c77a3de62e556ed7a7385618814fab8f0,0x4df1e42d4496cea4e90d24a3e66e824f114abbfb,0x0028622850fd75a0d9e4060e8e006e449c8ec237d4e22b6915d660e5140dde5d,7.134462804458488",
    "bk:0,2,1554410512",
    "fx:0,0.04951",
    "tx:1,0x02ea7c8f61f74e523e7fbaaa3961035adddd954a,0x4df1e42d4496cea4e90d24a3e66e824f114abbfb,0x10cf989c14128df45dd8d5788f7759460eb174fa9c9f36574258d045a71ce0e8,6.01100420129931",
    "tx:1,0x02ea7c8f61f74e523e7fbaaa3961035adddd954a,0xf2bd58c97232639aa2bc18b034a7bd0f50fd0eb9,0x3671e375652b7ad357c1ad1fcaf9741a928f5a9d98fc7dd3eab7ff7f290fcb44,9.082468917757293",
    "tx:1,0x4df1e42d4496cea4e90d24a3e66e824f114abbfb,0x02ea7c8f61f74e523e7fbaaa3961035adddd954a,0x82aba3bc95e9a12cb05fec1f4dda1d0a40825850019f4c1c22b2064405952e70,0.30509855438486166",
    "tx:1,0x02ea7c8f61f74e523e7fbaaa3961035adddd954a,0xcf0c6e6c77a3de62e556ed7a7385618814fab8f0,0x6fd4d1e44a54f92855757fca8fdd64c201da24441d238f246c6c672b9da2aa70,1.3606670501528284",
    "bk:1,4,1554413433",
    "fx:1,0.067675",
    "tx:2,0x6887d050357a1f466637cac4f8571d86ffbd9372,0xf2bd58c97232639aa2bc18b034a7bd0f50fd0eb9,0x056e002eebc1422b646d5d09f888f423837f7cfc74f6833478197b1d76d31026,9.842559175516218",
    "tx:2,0x4df1e42d4496cea4e90d24a3e66e824f114abbfb,0x02ea7c8f61f74e523e7fbaaa3961035adddd954a,0x828ea23f26291a37a49f59096d831fa3d6e81b588f41695a984b71bd0ad9c5fd,5.806409072178008",
    "bk:2,2,1554416220",
    "fx:2,0.08332"
  )).flatten

  private type AddressHash = String
  private type BlockNumber = Int

  private val bk = "bk"
  private val tx = "tx"
  private val fx = "fx"

  private def getDataFromRow(row: String): Array[String] = row
    .split(":")(1)
    .split(",")

  private def toBlock(block: String): Block = {
    val blockData = getDataFromRow(block)
    Block(blockData(0).toInt, blockData(1).toInt, blockData(2).toInt)
  }

  private def toTransaction(tx: String): Transaction = {
    val txData = getDataFromRow(tx)
    Transaction(txData(0).toInt, txData(1), txData(2), txData(3), txData(4).toDouble)
  }

  private def toFiatExchange(fx: String): FiatExchange = {
    val fxData = getDataFromRow(fx)
    FiatExchange(fxData(0).toInt, fxData(1).toDouble)
  }

  private def inputsToGroups(xs: List[String]): Groups = {
    val groups = xs.groupBy(x => x.split(":")(0))
    Groups(
      groups(bk).map(toBlock),
      groups(tx).map(toTransaction),
      groups(fx).map(toFiatExchange)
    )
  }

  private def addFiatAndTimeStampToTransaction(bks: List[Block], fxs: List[FiatExchange])(tx: Transaction): TransactionWithFullInfo = {
    val blockNumber = tx.block_number
    val exchangeRate = fxs.find(_.block_number == blockNumber)
    val fiatValue = exchangeRate.get.exchange_rate * tx.value

    val timestamp = bks.find(_.block_number == blockNumber).get.timestamp

    TransactionWithFullInfo(
      tx.sender_address,
      tx.recipient_address,
      tx.transaction_id,
      fiatValue,
      timestamp
    )
  }

  private def addAllInfoToTransaction(data: Groups): List[TransactionWithFullInfo] = {
    val txs = data.txs
    val fxs = data.fxs
    val bks = data.bks
    txs.map(addFiatAndTimeStampToTransaction(bks, fxs))
  }

  private def aggregateTxsForBlock(txs: List[TransactionWithFullInfo], timestamp: BlockNumber): Map[AddressHash, Double] = {
    val emptyMap = Map(): Map[AddressHash, Double]
    txs.foldLeft(emptyMap)((balanceMap, tx) => {
      val fiat_value = tx.fiat_value
      val sender_address = tx.sender_address
      val recipient_address = tx.recipient_address

      // check for case where address pays itself
      if (sender_address == recipient_address) {
        balanceMap
      } else {
        val oldSenderValue = balanceMap.getOrElse(sender_address, 0.0)
        val oldRecipientValue = balanceMap.getOrElse(recipient_address, 0.0)

        balanceMap +
          (sender_address -> (oldSenderValue - fiat_value)) +
          (recipient_address -> (oldRecipientValue + fiat_value))
      }
    })
  }

  private def toBlocks(txs: List[TransactionWithFullInfo]) =
    txs
      .groupBy(tx => tx.timestamp) // here I could like round to some range (depends on the granularity we want)
      .map(x => (x._1, aggregateTxsForBlock(x._2, x._1)))

  private def formatAnswerNicely(horribleRawForm: Map[Int, Map[AddressHash, Double]]): Json = {
    case class BalanceChange(address: String, balance_change: Double)
    case class BalanceChanges(timestamp: Int, balance_changes: Seq[BalanceChange])

    def toBalanceChange(x: (AddressHash, Double)) = BalanceChange(x._1, x._2)

    horribleRawForm
      .toSeq
      .map(x => {
        val (timestamp, rest) = x
        val balanceChanges = rest.toSeq.map(toBalanceChange)
        BalanceChanges(timestamp, balanceChanges)
      })
      .asJson
  }

  def main(): Unit = {
    // I want to use something like lodash's compose here
    val rawAnswer = toBlocks(addAllInfoToTransaction(inputsToGroups(inputs)))
    val json = formatAnswerNicely(rawAnswer)
    println(json)
  }

  main()
}
