/*
       __  ___     __ __    ___  _
      /  |/  /__ _/ //_/__ / _ \(_)___
     / /|_/ / _ `/ ,< / -_) // / / __/
    /_/  /_/\_,_/_/|_|\__/____/_/_/

*/

import math._
import scala.util._
import scala.io.StdIn._
import java.lang.String
import scala.collection.mutable.ListBuffer

// =========================== TYPES
object Types {
  type Position = (Int, Int)
  type TileMap = List[List[TileInfo]]
  type EntityId = Int
  type RobotId = EntityId
}
import Types._

object Player extends App {

  val Array(width, height) = (readLine split " ").filter(_ != "").map (_.toInt)

  (0 until 200)
    .foldLeft(InfoGlobal())((acc, current) => {

      // myScore: Amount of ore delivered
      val Array(myScore, opponentScore) = (readLine split " ").filter(_ != "").map (_.toInt)

      val tileMap: TileMap = (0 until height).toList
        .map(lineIndex => {
          val columns: List[String] = (readLine split " ").toList
          (0 until width)
            .foldLeft(List.empty[TileInfo])((acc, columnIndex) => {
              val position = (columnIndex, lineIndex)
              acc :+ TileInfo(columns(columnIndex*2), columns(columnIndex*2+1), position)
            })
        })



      val Array(entityCount, radarCooldown, trapCooldown) = (readLine split " ").filter(_ != "").map (_.toInt)

      val entities: List[Entity] = (0 until entityCount).toList
        .map { _ =>
          val Array(entityId, entityType, x, y, item) = (readLine split " ").filter(_ != "").map (_.toInt)
          Entity.entityFactory(entityId, entityType, x, y, item)
        }

      val emptyMap = List(("radar" -> List.empty), ("robot" -> List.empty), ("trap" -> List.empty)).toMap
      val mapWithElements = emptyMap ++ (entities.groupBy(e => e.entityType))


      val (radars :: robots :: traps :: Nil) = mapWithElements
        .toList
        .sortBy(_._1) // sort par ordre alphabétique ce qui garantit l'ordre suivant -> radars, robots, traps
        .map(_._2) // on recupère uniquement les listes de nos entités

      val gameMap = GameMap(width, height, tileMap, getRadars(radars))

      val mineRobots = Role.dispatcher(getMines(robots))

      Console.err.println(s"radars : ${radars}")
      Console.err.println(s"robots : ${robots}")
      Console.err.println(s"traps  : ${traps}")
      Console.err.println(s"mines  : ${mineRobots}")

      // instructions
      val infoGlobalesUpdated = mineRobots
        .foldLeft(acc)((infoAcc, currentRobot) => {
          val nAcc = currentRobot.role.action(currentRobot, gameMap, infoAcc)
          Console.err.println(s"old infos ------ $infoAcc")
          Console.err.println(s"new infos ------ $nAcc")
          nAcc
        })
      infoGlobalesUpdated
    })

  def getMines(robots: List[Entity]): List[Robot] = robots
    .map(_.asInstanceOf[Robot])
    .filter(_.mine)

  def getRadars(radars: List[Entity]): List[Radar] = radars
    .map(_.asInstanceOf[Radar])
}

// region entity

trait Entity {
  def entityType: String
}
object Entity {
  def entityFactory(entityId: Int, entityType: Int, x: Int, y: Int, item: Int): Entity =
    entityType match {
      case 0 => Robot(entityId, x, y, item, true)
      case 1 => Robot(entityId, x, y, item, false)
      case 2 => Radar(entityId, x, y)
      case 3 => Trap(entityId, x, y)
    }
}
case class Robot (entityId: Int, x: Int, y: Int, item: Int, mine: Boolean, role: Role = Chomeur) extends Entity {
  override val entityType = "robot"

  def hasEmptyInventory: Boolean = item == -1
  def hasRadar: Boolean = item == 2
  def hasTrap: Boolean = item == 3
  def hasOre: Boolean = item == 4
}
case class Radar (entityId: Int, x: Int, y: Int) extends Entity {
  override val entityType = "radar"
}
case class Trap (entityId: Int, x: Int, y: Int) extends Entity {
  override val entityType = "trap"
}
// endregion

// region role

sealed trait Role {
  def action(currentRobot: Robot, gameMap: GameMap, infoGlobal: InfoGlobal): InfoGlobal = {
    println("WAIT")
    infoGlobal
  }

}
case object Warder extends Role {
  override def action(currentRobot: Robot, gameMap: GameMap, infoGlobal: InfoGlobal): InfoGlobal = {
    val targetWards = targets(gameMap, infoGlobal)

    if (currentRobot.hasEmptyInventory && targetWards.nonEmpty) {
      println("REQUEST RADAR")
    } else if (currentRobot.hasRadar) {
      targetWards.headOption match {
        case Some(position) => println(s"DIG ${position._1} ${position._2}")
        case None => println("WAIT") // ce cas ne doit pas arriver
      }
    } else if (currentRobot.hasOre) {
      // on ramene le minerai a la base
      println(s"MOVE 0 ${currentRobot.y}")
    } else {
      // notre wardar n'a plus rien a faire, il peut miner
      Mineur.action(currentRobot, gameMap, infoGlobal)
    }
    infoGlobal
  }

  def targets(gameMap: GameMap, infoGlobal: InfoGlobal): List[Position] = infoGlobal
    .wardPositions
    .filter(position => !gameMap.getWardPositions.contains(position))
}
case object Mineur extends Role {

  override def action(currentRobot: Robot, gameMap: GameMap, infoGlobal: InfoGlobal): InfoGlobal = {
    // define target
    val infoGlobalUpdated: InfoGlobal = defineTarget(currentRobot, gameMap, infoGlobal)

    if (currentRobot.hasOre) {
      println(s"MOVE 0 ${currentRobot.y}")
    } else if (infoGlobalUpdated.oreTargets.haveMark(currentRobot)) {
      val position = infoGlobalUpdated.oreTargets.getTarget(currentRobot).get
      println(s"DIG ${position._1} ${position._2}")
    } else {
      println("WAIT")
    }

    Console.err.println(s" Mineur Info result------- $infoGlobalUpdated")
    infoGlobalUpdated
  }


  private def defineTarget(currentRobot: Robot, gameMap: GameMap, infoGlobal: InfoGlobal): InfoGlobal = {
    // on regarde si notre robot n'a pas déjà une cible
    if (infoGlobal.oreTargets.haveMark(currentRobot)) {
      // notre robot a deja du travail, mais on va quand meme verifier qu'il y a bien du minerai a sa position
      // on va vérifié aussi qu'il ne porte rien
      val mineraiPositions = gameMap.getOrePositions
      val targetOpt = infoGlobal.oreTargets.getTarget(currentRobot)
      targetOpt match {
        case Some(target) => {
          if (mineraiPositions.exists(_ == target)) {
            // il y a encore du minerai on ne fait rien
            infoGlobal
          } else {
            // in n'y a plus de minerai, on peut lui definir une nouvelle cible pour la prochaine iteration
            val unmark = infoGlobal.copy(oreTargets = RobotsTargets(infoGlobal.oreTargets.unmark(currentRobot)))
            defineTargetWithoutAnyCheck(currentRobot, gameMap, unmark)
          }
        }
        case None => infoGlobal
      }
    } else {
      defineTargetWithoutAnyCheck(currentRobot, gameMap, infoGlobal)
    }
  }

  private def defineTargetWithoutAnyCheck(currentRobot: Robot, gameMap: GameMap, infoGlobal: InfoGlobal): InfoGlobal = {
    // on cherche une cible
    val mineraiPositions = gameMap.getOrePositions
    val mineraiNonMarques = mineraiPositions
      .filter(position => infoGlobal.oreTargets.canMark(currentRobot, position))
    // on marque la position
    val targetPositionOpt = mineraiNonMarques.headOption
    targetPositionOpt match {
      case Some(targetPosition) => {
        // on trouve un position a cible => on met a jour infoGlobal avec la nouvelle target
        val mapp = infoGlobal.oreTargets.mark(currentRobot, targetPosition)
        val nm = infoGlobal.copy(oreTargets = RobotsTargets(mapp))
        nm
      }
      case None => infoGlobal // on a pas de nouvelle cible, on ne fait rien
    }
  }
}
case object Chomeur extends Role
object Role {
  def dispatcher(robots: List[Robot]): List[Robot] = {
    // je dis que le robot a l'id le plus petit est un warder
    val sortedRobots = robots
      .sortBy(_.entityId)
    val warderId = sortedRobots
      .head
      .entityId

    val mineurIds = sortedRobots
      .tail
      .map(_.entityId)

    robots
      .map {
        case warder if warder.entityId == warderId =>
          warder.copy(role = Warder)
        case mineur if mineurIds.contains(mineur.entityId) =>
          mineur.copy(role = Mineur)
        case chomeur => chomeur
      }
  }
}

// endregion role

// region game map

case class TileInfo(oreInfo: String, holeInfo: String, position: Position)

case class GameMap(width: Int, Height: Int, tileMap: TileMap, wards: List[Radar]) {
  def getOrePositions: List[Position] = tileMap
    .flatten
    .filter(current => {
      if (current.oreInfo != "?") {
        val nbOre = current.oreInfo.toInt
        nbOre > 0
      } else false
    })
    .map(tile => tile.position)

  def getWardPositions: List[Position] = wards
    .map(radar => (radar.x, radar.y))
}
// endregion


case class RobotsTargets(targets: Map[RobotId, Position]) {
  def mark(robot: Robot, position: Position): Map[RobotId, Position] =
    if (canMark(robot, position)) {
      Console.err.println("On peut marquer")
      targets ++ List(robot.entityId -> position).toMap
    } else {
      targets
    }

  def haveMark(robot: Robot): Boolean = targets.contains(robot.entityId)

  // on retire le robot des marquage pour toutes cibles
  def unmark(robot: Robot): Map[RobotId, Position] =
    targets.filter(elem => elem._1 != robot.entityId)

  def canMark(robot: Robot, position: Position): Boolean =
    !existPosition(position)

  // retourne vrai si la position est déjà marquée
  def existPosition(position: Position): Boolean =
    targets.keys
      .map(key => targets.get(key))
      .filter(_.isDefined)
      .map(_.get)
      .exists(current => current == position)

  def getTarget(robot: Robot): Option[Position] = targets.get(robot.entityId)
}

// region shared loop data

case class InfoGlobal(oreTargets: RobotsTargets, wardPositions: List[Position]) {

}
object InfoGlobal {
  val wards = List((5, 11), (10, 7), (16, 8), (18, 4), (20, 8))
  def apply(): InfoGlobal = new InfoGlobal(RobotsTargets(Map.empty[RobotId, Position]), wards)
}
// endregion