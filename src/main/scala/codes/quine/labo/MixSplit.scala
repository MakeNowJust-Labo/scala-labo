package codes.quine.labo

final case class MixSplit private (seed: Long, gamma: Long) {
  import MixSplit._

  def next: (Long, MixSplit) = {
    val seed1 = seed + gamma
    (mix64(seed1), new MixSplit(seed1, gamma))
  }

  def split: (MixSplit, MixSplit) = {
    val seed1 = seed + gamma
    val seed2 = seed1 + gamma
    (new MixSplit(seed2, gamma), new MixSplit(mix64(seed1), mixGamma(seed2)))
  }

  def left: MixSplit = {
    val seed2 = seed + gamma * 2
    new MixSplit(seed2, gamma)
  }

  def right: MixSplit = {
    val seed1 = seed + gamma
    val seed2 = seed1 + gamma
    new MixSplit(mix64(seed1), mixGamma(seed2))
  }
}

object MixSplit {
  final val GOLDEN_GAMMA: Long = 0x9E3779B97F4A7C15L

  def apply(seed: Long, gamma: Long): MixSplit =
    new MixSplit(seed, gamma | 1)

  def get(seed: Long): MixSplit =
    new MixSplit(mix64(seed), mixGamma(seed + GOLDEN_GAMMA))

  private def mix64(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 33)) * 0xFF51AFD7ED558CCDL
    val z2 = (z1 ^ (z1 >>> 33)) * 0xC4CEB9FE1A85EC53L
    z2 ^ (z2 >>> 33)
  }

  private def mixGamma(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 30)) * 0xBF58476D1CE4E5B9L
    val z2 = (z1 ^ (z1 >>> 27)) * 0x94D049BB133111EBL
    val z3 = (z2 ^ (z2 >>> 31)) | 1
    val n = java.lang.Long.bitCount(z3 ^ (z3 >>> 1))
    if (n >= 24) z3 else z3 ^ 0xAAAAAAAAAAAAAAAAL
  }
}
