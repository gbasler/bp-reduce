package bpReduce
package transformations

trait TransformerFactory {
  /**
   * @return new transformer, ready to perform first transformation.
   */
  def create: Transformer
}
