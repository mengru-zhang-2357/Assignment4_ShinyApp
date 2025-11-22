library(shiny)
library(ggthemes)

# Set up the UI
bibUI <- function(id, title){
  tabPanel(
    title,
    p("Choudhary, N., Brewis, A., Wutich, A., & Udas, P. B. (2020). Sub-optimal household water access is associated with greater risk of intimate partner violence against women: evidence from Nepal. Journal of Water and Health, 18(4), 579–594. https://doi.org/10.2166/wh.2020.024"),
    
    br(),
    
    p("Cooper-Vince, C. E., Kakuhikire, B., Vorechovska, D., McDonough, A. Q., Perkins, J., Venkataramani, A. S., Mushavi, R. C., Baguma, C., Ashaba, S., Bangsberg, D. R., & Tsai, A. C. (2017). Household water insecurity, missed schooling, and the mediating role of caregiver depression in rural Uganda. Global Mental Health, 4(e15). https://doi.org/10.1017/gmh.2017.14"),
    
    br(),
      
    p("Graham, J. P., Hirai, M., & Kim, S.-S. (2016). An Analysis of Water Collection Labor among Women and Children in 24 Sub-Saharan African Countries. PLOS ONE, 11(6), e0155981. https://doi.org/10.1371/journal.pone.0155981"),
    
    br(),
      
    p("Mungekar, N. (2022, October 24). Why is fetching water considered a woman’s job? Www.un-Ihe.org. https://www.un-ihe.org/why-fetching-water-considered-womans-job"),
    
    br(),
    
    p("World Health Organization, & United Nations Children’s Fund (Eds.). (2025). Progress on household drinking-water, sanitation and hygiene 2000-2024: Special focus on inequalities. United Nations Children’s Fund (UNICEF) and World Health Organization. Licence: CC BY-NC-SA 3.0 IGO. https://www.who.int/publications/m/item/progress-on-household-drinking-water--sanitation-and-hygiene-2000-2024--special-focus-on-inequalities")
  )
}

bibServer <- function(id){
  moduleServer(id, function(input, output, session){
    thematic::thematic_shiny()
  })
}