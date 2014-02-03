package bpReduce
package transformations
package reduction

trait TransformerFactory {
  /**
   * @return new transformer, ready to perform first transformation.
   */
  def create: Transformer
}
