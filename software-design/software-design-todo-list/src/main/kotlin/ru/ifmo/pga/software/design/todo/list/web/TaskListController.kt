package ru.ifmo.pga.software.design.todo.list.web

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import ru.ifmo.pga.software.design.todo.list.entity.TaskList
import ru.ifmo.pga.software.design.todo.list.service.TaskListService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Controller
class TaskListController @Autowired constructor(
    private val taskListService: TaskListService,
) {

    @RequestMapping(value = ["/lists"], method = [RequestMethod.GET])
    fun getLists(model: Model): String {
        model.addAttribute("taskLists", taskListService.findAll())
        model.addAttribute("newTaskList", TaskList())
        return "task_lists"
    }

    @RequestMapping(value = ["/lists/add-task-list"], method = [RequestMethod.POST])
    fun addList(
        @RequestParam(name = "name", required = true, defaultValue = "Some Task List") name: String,
        @RequestParam(name = "description", required = false) description: String?,
    ): String {
        taskListService.save(TaskList().apply {
            this.name = name
            this.description = description
        })
        return "redirect:/lists"
    }

    @RequestMapping(value = ["/lists/delete"], method = [RequestMethod.POST])
    fun deleteList(
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        taskListService.remove(id)
        return "redirect:/lists"
    }
}
