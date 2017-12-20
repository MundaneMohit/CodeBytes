SET FEEDBACK OFF
SET LINESIZE 1000
SET HEADING OFF
SET PAGESIZE 0
SET TRIMSPOOL ON

---------------------------------------------
--Create Agents Table
---------------------------------------------
drop table ccdw_agents_cur_month;

create table ccdw_agents_cur_month
as
select SALES_REPTV_ID,EOM_REPTV_SAP_ID,
         EOM_REPTV_FULL_NM 
from SALES_CHNL_DIM@to_ccdwp
where SALES_CHNL_BEG_EFF_DT= trunc(trunc(sysdate)-5, 'MONTH')
         and EOM_REPTV_SAP_ID != '~';  

-----------------------------------------------
--Create Agent_Promo_Usage table
-----------------------------------------------

drop table agent_promo_mailer;

create table agent_promo_mailer 
as
select * from (select     ap.SAP_ID ,
                        mc.CUID ,
                        ap.DISCOUNT,
                        ap.IN_DATE,
                        sum(PROMO_COUNT) as PC ,
                        ap.Metric 
                        from (      select     IN_Date,
                                        metric as Metric,
                                        AccountNumber as Account_Number,
                                        DISCOUNT_CODE as Discount_Code,
                                        decode(lhpus.discount_desc,    'HSI CredClos-$300/3yr-15M+','Discretionary Closing Discount $300',
                                                                                        'HSI CredClos-$200/2yr-3.0M+','Discretionary Closing Discount $200',
                                                                                        'HSI CredClos-$100/1yr-1.5M+','Discretionary Closing Discount $100',
                                                                                        'HSI CredClos-$50/1yr-1.5M+','Discretionary Closing Discount $50',
                                                                                        'HSI CredClos-$25/1yr-1.5M+','Discretionary Closing Discount $25',
                                                                                        lhpus.discount_desc) 
                                         as Discount,
                                        asap.EMP_SAP_ID as SAP_ID,
                                        ens_agent_name as Agent_nm,
                                        ens_agent_name as Agent,
                                        ens_agent_name as Agent_fullname,
                                        decode(lhpus.ens_common_channel_group,'Field Sales','DOOR TO DOOR',
                                                        lhpus.ens_common_channel_group) 
                                        as Channel,
                                        sum(Count_Promos) as Promo_Count,
                                        'CTL' as Legacy
                                        from lctl_hsi_promo_usage_stage lhpus
                                        left join ab37179.Agent_sap_id asap
                                                    on initcap(SUBSTR(ens_agent_name, INSTR(ens_agent_name,' ')+1, LENGTH(ens_agent_name)))=asap.EMP_LST_NM
                                                    and initcap(SUBSTR(ens_agent_name,1, INSTR(ens_agent_name,' ')-1))=asap.EMP_FRST_NM
                                        where DISCOUNT_CODE in ('820K', '821K','823K', '820L', '821L')
                                                    and  IN_DATE>= trunc(trunc(sysdate)-5, 'MONTH')
                                                    and lhpus.ens_common_channel_group='Call Center/Solutions'
                                                    group by IN_Date,metric,region_name,AccountNumber,
                                                                 DISCOUNT_CODE,Discount_desc,ens_common_channel_group,
                                                                 asap.EMP_SAP_ID,ens_agent_name
                                       union all
                                       select PP_Passdt as In_Date,
                                                decode(lpuc.Metric,'Mig_Tran_In','Mig_Inward',
                                                                            lpuc.Metric)
                                                as Metric,
                                                CUSTOMER_ID as Account_Number,
                                                disfid as Discount_code,
                                                decode(lpuc.DESCRIPTION,'HSI Closing Offer 3YR - $300','Discretionary Closing Discount $300',
                                                                                        'HSI Closing Offer 2YR - $200','Discretionary Closing Discount $200',
                                                                                        'HSI Closing Offer 1YR - $100','Discretionary Closing Discount $100',
                                                                                        'Discretionary Discount $100','Discretionary Closing Discount $100',
                                                                                        'Discretionary Discount $300 ','Discretionary Closing Discount $300',
                                                                                        'Discretionary Discount $200','Discretionary Closing Discount $200',
                                                                                        '$50 credit closer','Discretionary Closing Discount $50',   
                                                                                        '$25 credit closer','Discretionary Closing Discount $25',
                                                                                        lpuc.DESCRIPTION) 
                                                            as Discount,
                                                scd.EOM_REPTV_SAP_ID as SAP_ID,
                                                Agent_Name as Agent,
                                                (case Agent_Name when '~,~' then scd.EOM_REPTV_FULL_NM else Agent_Name end) as Agent_fullname,
                                                scd.EOM_REPTV_FULL_NM,
                                                decode(lpuc.Call_center_evp_1_desc,    '~','OTHER',
                                                            'SBG ALTERNATIVE CHANNELS','Alternative Markets',
                                                            'MID MARKET CHANNEL','OTHER',
                                                             lpuc.Call_center_evp_1_desc) 
                                                as Channel,
                                                SUM(Count_promos) as Promo_count,   
                                                'Q' as Legacy   
                                    from lq_promo_usage_cur lpuc
                                    left join ab37179.ccdw_agents_cur_month scd
                                            on lpuc.SALES_CODE=scd.SALES_REPTV_ID
                                    WHERE
                                    (
                                        BTN  Is Not Null  
                                         AND
                                        PRODUCT  =  'INTERNET'   
                                        AND
                                        PROMO_ADDED  =  'Y'  
                                        AND
                                        PP_PASSDT  >=  trunc(trunc(sysdate)-5, 'MONTH')
                                        and disfid in ('D20401', 'D20402', 'D20403', 'D21401', 'D21402', 'D21403', 'D20125', 'D20150')
                                        and lpuc.Call_center_evp_1_desc='CALL CENTER/SOLUTIONS'
                                )
                                     GROUP BY PP_Passdt,Metric,
                                                CUSTOMER_ID,disfid,lpuc.DESCRIPTION,
                                                Call_center_evp_1_desc,scd.EOM_REPTV_SAP_ID,
                                                Agent_Name,scd.EOM_REPTV_FULL_NM 
                                    order by In_Date desc
                         ) ap
left join mnet_cur mc
	on ap.SAP_ID=mc.SAP_ID
where mc.CUID is not null
    and ap.SAP_ID is not null
group by ap.IN_DATE,ap.SAP_ID,mc.CUID,ap.DISCOUNT,ap.Metric
order by  ap.IN_DATE asc);
--output file location
		 
spool "C:\Users\AB37179\Documents\My Projects\agt_tst\Base File\Agent_Promo_Usage_.txt";
		 
		 
 select 
'SAP_ID'||'|'||'CUID'||'|'||'PROMOTION'||'|'||'DATE_USED'||'|'||'QTY_USED'||'|'||'METRIC'
from dual
union all
 select     SAP_ID ||'|'||
    CUID ||'|'||
    DISCOUNT ||'|'||
    IN_DATE ||'|'||
    PC ||'|'||
    Metric
from agent_promo_mailer;


spool off;

exit;