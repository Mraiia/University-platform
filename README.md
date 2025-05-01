import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.concurrent.Await
import scala.concurrent.duration._

// Токен (Token)
case class Token(amount: Double) {
  def +(other: Token): Token = Token(this.amount + other.amount)
  def -(other: Token): Token = Token(this.amount - other.amount)
  def >=(other: Token): Boolean = this.amount >= other.amount
  def isNonNegative: Boolean = this.amount >= 0
}

// Торгівля (Trading)
trait Trading {
  def sell(amount: Token): Unit
  def buy(amount: Token): Unit
}

// Людина (Human)
abstract class Human(val name: String, val age: Int, val gender: String, val address: String) extends Trading {
  var tokenBalance: Token = Token(0.0)

  override def buy(amount: Token): Unit = {
    tokenBalance = tokenBalance + amount
    println(s"$name купив ${amount.amount} токенів.")
  }

  override def sell(amount: Token): Unit = {
    if (tokenBalance >= amount) {
      tokenBalance = tokenBalance - amount
      println(s"$name продав ${amount.amount} токенів.")
    } else {
      println(s"$name не вистачає токенів для продажу.")
    }
  }
}

// Студент (Student)
class Student(name: String, age: Int, gender: String, address: String, var group: String)
  extends Human(name, age, gender, address) {

  var _name: String = name
  var grade: Option[Double] = None
  var scholarship: Double = 0.0
  var enrolledCourses: List[Course] = List()

  def evaluateTeacher(teacher: Teacher, rating: Int): Unit = {
    teacher.receiveRating(rating)
  }

  def buyCourse(course: Course, platform: Platform): Boolean = {
    if (course.isActive(platform.currentMonth)) {
      val transaction = new Transaction(this, platform.exchange, Token(course.price))
      if (transaction.execute()) {
        enrolledCourses = enrolledCourses :+ course
        course.addStudent(this)
        println(s"$name зареєструвався на курс ${course.name}.")
        true
      } else {
        println(s"$name не вистачає токенів для покупки курсу ${course.name}.")
        false
      }
    } else {
      println(s"Курс ${course.name} неактивний у поточному місяці.")
      false
    }
  }

  def generateGrade(): Unit = {
    grade = Some(Random.nextInt(42) + 59) // Оцінка від 59 до 100
  }

  def calculateScholarship(): Unit = {
    grade match {
      case Some(g) =>
        scholarship = g match {
          case g if g >= 90 => 2.0 * g
          case g if g >= 80 => 1.2 * g
          case g if g >= 70 => 0.9 * g
          case g if g >= 60 => 0.6 * g
          case _ => 0.0
        }
        println(s"Стипендія для студента $name: $scholarship.")
      case None => scholarship = 0.0
    }
  }

  def showDetails(): Unit = {
    println(s"Студент: ${_name}, Група: ${group}, Баланс токенів: ${tokenBalance.amount}, Оцінка: ${grade.getOrElse("Немає")}, Стипендія: $scholarship")
  }
}

// Викладач (Teacher)
class Teacher(name: String, age: Int, gender: String, address: String, var salary: Double)
  extends Human(name, age, gender, address) {

  var rating: Double = 0.0
  var studentsCount: Int = 0
  var courses: List[Course] = List()

  def addCourse(course: Course, platform: Platform): Unit = {
    if (tokenBalance >= Token(course.price) && course.isActive(platform.currentMonth)) {
      val transaction = new Transaction(this, platform.exchange, Token(course.price))
      if (transaction.execute()) {
        courses = courses :+ course
        println(s"$name додав курс ${course.name}.")
      } else {
        println(s"$name не вистачає токенів для додавання курсу ${course.name}.")
      }
    } else {
      println(s"$name не може додати курс ${course.name}: недостатньо токенів або курс неактивний.")
    }
  }

  def receiveRating(rating: Int): Unit = {
    val newRating = (this.rating * studentsCount + rating) / (studentsCount + 1)
    studentsCount += 1
    this.rating = newRating
  }

  def calculateSalary(): Unit = {
    rating match {
      case r if r >= 4.0 => salary *= 1.6
      case r if r >= 3.0 => salary *= 1.2
      case r if r >= 2.0 => salary *= 0.8
      case r if r >= 1.0 => salary *= 0.6
      case _ => salary = 0.0
    }
    println(s"Зарплата викладача $name: $salary.")
  }

  def showDetails(): Unit = {
    println(s"Викладач: $name, Вік: $age, Зарплата: $salary, Рейтинг: $rating, Кількість студентів: $studentsCount")
  }
}

// Курс (Course)
class Course(val name: String, val category: String, val price: Double, val period: Int, val startMonth: Int) {
  var students: List[Student] = List()
  val endMonth: Int = startMonth + period

  def isActive(currentMonth: Int): Boolean = currentMonth >= startMonth && currentMonth <= endMonth

  def addStudent(student: Student): Unit = {
    students = students :+ student
    println(s"Студент ${student.name} доданий до курсу $name.")
  }

  def evaluateStudent(student: Student, currentMonth: Int): Unit = {
    if (currentMonth == endMonth) {
      student.generateGrade()
      student.calculateScholarship()
      println(s"Студент ${student.name} оцінений на курсі $name.")
    }
  }
}

// Біржа (Exchange)
class Exchange extends Trading {
  var fiatBalance: Double = 10000.0

  def priceOfToken(): Double = fiatBalance / 1000.0

  override def buy(amount: Token): Unit = {
    fiatBalance -= amount.amount * priceOfToken()
    println(s"Біржа купила ${amount.amount} токенів.")
  }

  override def sell(amount: Token): Unit = {
    fiatBalance += amount.amount * priceOfToken()
    println(s"Біржа продала ${amount.amount} токенів.")
  }
}

// Транзакція (Transaction)
class Transaction(from: Trading, to: Trading, amount: Token) {
  def execute(): Boolean = {
    val future = Future {
      synchronized {
        if (from.isInstanceOf[Human] && from.asInstanceOf[Human].tokenBalance >= amount) {
          from.sell(amount)
          to.buy(amount)
          true
        } else {
          println(s"Транзакція не вдалася: недостатньо токенів у ${from.getClass.getSimpleName}.")
          false
        }
      }
    }
    Await.result(future, 5.seconds)
  }
}

// Платформа (Platform)
class Platform {
  var exchange: Exchange = new Exchange()
  var currentMonth: Int = 1
  var students: List[Student] = List()
  var teachers: List[Teacher] = List()
  var courses: List[Course] = List()

  def addStudent(student: Student): Unit = students = students :+ student
  def addTeacher(teacher: Teacher): Unit = teachers = teachers :+ teacher
  def addCourse(course: Course): Unit = courses = courses :+ course

  def processMonth(): Unit = {
    println(s"=== Обробка місяця $currentMonth ===")
    teaching()
    currentMonth += 1
  }

  def teaching(): Unit = {
    println("Розпочинається навчальний процес...")

    // Паралельне оцінювання студентів
    val evaluationFutures = for {
      course <- courses if course.isActive(currentMonth)
      student <- course.students
    } yield Future {
      course.evaluateStudent(student, currentMonth)
    }

    // Чекаємо завершення всіх оцінювань
    Await.result(Future.sequence(evaluationFutures), 10.seconds)

    // Оцінювання викладачів студентами
    for (student <- students; teacher <- teachers) {
      val rating = Random.between(1, 6)
      student.evaluateTeacher(teacher, rating)
    }

    // Нарахування зарплат
    for (teacher <- teachers) {
      teacher.calculateSalary()
    }

    println("Навчальний процес завершено.")
  }

  def showPlatformDetails(): Unit = {
    println(s"Платформа працює. Поточний місяць: $currentMonth.")
  }
}

// Демонстрація роботи
object UniversityManagementSystem extends App {
  val platform = new Platform()

  val teacher1 = new Teacher("Dr. John", 45, "Male", "123 Street", 1000)
  val teacher2 = new Teacher("Dr. Emily", 40, "Female", "789 Avenue", 1200)

  val student1 = new Student("Alice", 22, "Female", "456 Avenue", "Group 1")
  val student2 = new Student("Bob", 23, "Male", "123 Street", "Group 2")

  val course1 = new Course("Scala Programming", "Programming", 50, 6, startMonth = 1)
  val course2 = new Course("Database Management", "Database", 40, 4, startMonth = 2)

  platform.addTeacher(teacher1)
  platform.addTeacher(teacher2)
  platform.addStudent(student1)
  platform.addStudent(student2)
  platform.addCourse(course1)
  platform.addCourse(course2)

  // Поповнення балансу
  student1.buy(Token(100))
  student2.buy(Token(50))
  teacher1.buy(Token(200))
  teacher2.buy(Token(150))

  // Реєстрація на курси
  student1.buyCourse(course1, platform)
  student2.buyCourse(course2, platform)

  // Додавання курсів викладачами
  teacher1.addCourse(course1, platform)
  teacher2.addCourse(course2, platform)

  // Обробка кількох місяців
  platform.processMonth()
  platform.processMonth()

  // Виведення деталей
  student1.showDetails()
  student2.showDetails()
  teacher1.showDetails()
  teacher2.showDetails()
  platform.showPlatformDetails()
}
