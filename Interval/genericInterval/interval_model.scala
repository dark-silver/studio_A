package o1.time
import scala.math._


/** Each instance of the class `Interval` represents an interval --- an inclusive range ---
  * on a time scale. An interval has a "start moment" and an "end moment", represented
  * as `Moment` objects. An interval always contains at least a single moment.
  *
  * An interval object may be used to represent a range of years, a range of months, etc.,
  * as desired
  *
  * An `Interval` object is immutable after it has been created. That is, its state
  * can not be changed in any way.
  *
  * @see [[Moment]]
  * @param start  the start of the interval (that is, the first moment included in the interval)
  * @param end    the end of the interval (that is, the last moment included in the interval);
  *               equal to or higher than `start` */
class Interval(val start: Moment, val end: Moment) {


  /** Returns the length of the interval. For instance, if the interval represents
    * the range of years from 2000 to 2013, returns 13. The length of an interval
    * will always be at least 0 (which is the case if `start` equals `end`). */
  def length = this.end.distance(this.start)


  /** Returns a textual description of the interval. This is a string whose length depends on
    * the length of the interval as follows.
    *
    * If the length of the interval is 0, returns a string containing just the single moment
    * contained in the interval. E.g., the interval from 2013 to 2013 is represented by the
    * string `"2013"`.
    *
    * If the length of the interval is above 0 but no more than 50, returns a string consisting
    * of the start moment and the end moment separated by a number of hyphens equal to the length
    * of the interval. For instance, the interval from 2013 to 2014 is represented by the string
    * `"2013-2014"` and the interval from 2000 to 2013 is represented by the string
    * `"2000-------------2013"`.
    *
    * If the length of the interval is more than 50, uses three dots instead of hyphens.
    * For instance, the interval from 1900 to 2013 is represented by the string `"1900...2013"`. */
  override def toString =
    if (this.length <= 0)
      this.start.toString
    else if (this.length <= 50)
      this.start + "-" * this.length + this.end
    else
      this.start + "..." + this.end


  /** Determines whether this interval is later than the given moment. This is only deemed to
    * be the case if the entire interval comes after the given moment on the time scale. */
  def isLaterThan(moment: Moment) = this.start.isLaterThan(moment)


  /** Determines whether this interval is later than the given interval. This is only deemed
    * to be the case if this entire interval comes after the given interval on the time scale.
    * That is, no overlap is allowed. */
  def isLaterThan(another: Interval): Boolean = this.isLaterThan(another.end)


  /** Determines whether the given moment is inside this interval. (An interval also includes
    * its start and end moments.) */
  def contains(moment: Moment) = !this.isLaterThan(moment) && !moment.isLaterThan(this)


  /** Determines whether this interval contains the given interval. This is the case if and
    * only if all moments within the other interval are contained within this interval. */
  def contains(another: Interval): Boolean = this.contains(another.start) && this.contains(another.end)


  /** Determines whether this interval overlaps (intersects) the given interval.
    * This is the case if (and only if) one or more of the moments within the other
    * interval are contained within this interval.
    *
    * Note: If one interval is entirely contained within the other, it counts
    * as overlapping, as does the case where one interval ends exactly where the
    * other one begins. */
  def overlaps(another: Interval) = !this.isLaterThan(another) && !another.isLaterThan(this)


  /** Creates, and returns a reference to, a new `Interval` object that represents the union
    * of this interval with the given interval. That is, the starting moment of the new interval
    * is the starting moment of one of the two original intervals, whichever is earlier.
    * Similarly, the end moment of the new interval is the later of the two original end moments.
    *
    * The two original intervals may overlap, but are not required to do so.
    *
    * Examples: The union of the interval from 1995 to 2003 with the interval from 2000 to 2013 is
    * a new interval from 1995 to 2013. The union of the interval from 2000 to 2001 with the interval
    * from 1995 to 1997 is a new interval from 1995 to 2001. */
  def union(another: Interval) = new Interval(this.start.earlier(another.start), this.end.later(another.end))


  /** Creates, and returns a reference to, a new `Interval` object that represents the intersection
    * of this interval with the given interval. That is, the starting moment of the new interval
    * is the starting moment of one of the two original intervals, whichever is later. Similarly,
    * the end moment of the new interval is the earlier of the two original end moments.
    *
    * However, this method only produces a new interval in case the two original intervals overlap.
    * In that case, the new interval is wrapped inside a `Some` object. If no intersection exists, ̀
    * the method returns `None` instead.
    *
    * Examples: The intersection of the interval from 1995 to 2003 with the interval from 2000 to
    * 2013 is a new interval from 2000 to 2003. The intersection of the interval from 2000 to 2001
    * with the interval from 1995 to 1997 does not exist.
    *
    * @see overlaps
    * @see union   */
  def intersection(another: Interval) =
    if (this.overlaps(another)) {
      val intersectingBit = new Interval(this.start.later(another.start), this.end.earlier(another.end))
      Some(intersectingBit)
    } else {
      None
    }

}