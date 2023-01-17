package ru.ifmo.pga.software.design.todo.list.web

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import ru.ifmo.pga.software.design.todo.list.entity.Task
import ru.ifmo.pga.software.design.todo.list.entity.enums.Status
import ru.ifmo.pga.software.design.todo.list.service.TaskService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Controller
class TaskController @Autowired constructor(
    private val taskService: TaskService,
) {
    @RequestMapping(value = ["/list"], method = [RequestMethod.GET])
    fun getList(
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        model.addAttribute("tasks", taskService.findByTaskListId(id))
        model.addAttribute("TO_DO", Status.TO_DO)
        model.addAttribute("DONE", Status.DONE)
        return "tasks"
    }

    @RequestMapping(value = ["/list/add-task"], method = [RequestMethod.POST])
    fun addTask(
        @RequestParam(name = "taskListId", required = true) taskListId: Long,
        @RequestParam(name = "name", required = true, defaultValue = "Some Task") name: String,
        @RequestParam(name = "description", required = false) description: String?,
    ): String {
        taskService.save(Task().apply {
            this.taskListId = taskListId
            this.name = name
            this.description = description
        })
        return "redirect:/list?id=${taskListId}"
    }

    @RequestMapping(value = ["/list/delete"], method = [RequestMethod.POST])
    fun deleteTask(
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        val task = taskService.findById(id) ?: error("Task wasn't found")
        taskService.remove(id)
        return "redirect:/list?id=${task.taskListId}"
    }

    @RequestMapping(value = ["/list/change-status"], method = [RequestMethod.POST])
    fun changeTaskStatus(
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        val task = taskService.findById(id) ?: error("Task wasn't found")
        taskService.save(
            task.apply {
                status = when (task.status) {
                    Status.TO_DO -> Status.DONE
                    Status.DONE -> Status.TO_DO
                }
            })
        return "redirect:/list?id=${task.taskListId}"
    }
}
